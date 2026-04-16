;;; wakatime.scm — WakaTime plugin for Helix
;;;
;;; Tracks coding activity by sending heartbeats to wakatime-cli on
;;; document open, save, focus-lost, and idle-after-edit events.
;;; Idle heartbeats use a generation-based debounce: each edit bumps a
;;; per-file counter, and the delayed callback only fires if the counter
;;; hasn't changed (i.e. no further edits arrived within the delay window).

(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin steel/core/result)
(require-builtin steel/process)
(require-builtin steel/strings)
(require-builtin steel/time)

(provide install-wakatime-plugin!)

;; ---------------------------------------------------------------------------
;; Configuration & state
;; ---------------------------------------------------------------------------

(define *wakatime-installed* #f)
;; path -> generation counter, used for debouncing idle heartbeats
(define *wakatime-doc-generations* (hash))
(define *wakatime-cli* "wakatime-cli")
(define *wakatime-editor-name* "helix")
;; cached on first successful detection
(define *wakatime-editor-version* #f)
;; cached after first build so the string isn't rebuilt per heartbeat
(define *wakatime-plugin-string* #f)
(define *wakatime-plugin-version* "0.1.0")
(define *wakatime-idle-delay-ms* 2000)
;; Throttle: skip repeat activity heartbeats for the same file within this window.
;; Write events always go through regardless.
(define *wakatime-throttle-ms* 120000)
;; path -> last heartbeat timestamp (ms)
(define *wakatime-last-heartbeat-times* (hash))

;; ---------------------------------------------------------------------------
;; Logging helpers
;; ---------------------------------------------------------------------------

(define (log-info message)
  (log::info! (string-append "[wakatime] " message)))

(define (log-warn message)
  (log::warn! (string-append "[wakatime] " message)))

;; ---------------------------------------------------------------------------
;; Editor version detection
;; ---------------------------------------------------------------------------

;; Parse "helix 24.7" → "24.7"
(define (extract-version output)
  (let ([parts (split-many (trim output) " ")])
    (if (> (length parts) 1)
        (list-ref parts 1)
        "unknown")))

;; Try "hx --version", then "helix --version"; cache the result.
(define (editor-version)
  (if *wakatime-editor-version*
      *wakatime-editor-version*
      (let ([commands '("hx" "helix")])
        (let loop ([remaining commands])
          (if (null? remaining)
              (begin
                (set! *wakatime-editor-version* "unknown")
                "unknown")
              (let ([spawned (spawn-process (with-stdout-piped (command (car remaining)
                                                                        (list "--version"))))])
                (if (Ok? spawned)
                    (let ([output (wait->stdout (Ok->value spawned))])
                      (if (Ok? output)
                          (let ([version (extract-version (Ok->value output))])
                            (set! *wakatime-editor-version* version)
                            version)
                          (loop (cdr remaining))))
                    (loop (cdr remaining)))))))))

;; Build the --plugin flag value, e.g. "helix/24.7 helix-wakatime/0.1.0".
;; Cached after first call.
(define (wakatime-plugin-string)
  (or *wakatime-plugin-string*
      (let ([s (string-append *wakatime-editor-name*
                              "/"
                              (editor-version)
                              " "
                              *wakatime-editor-name*
                              "-wakatime/"
                              *wakatime-plugin-version*)])
        (set! *wakatime-plugin-string* s)
        s)))

;; ---------------------------------------------------------------------------
;; Document helpers
;; ---------------------------------------------------------------------------

(define (doc->path-string doc-id)
  (editor-document->path doc-id))

;; ---------------------------------------------------------------------------
;; Debounce: generation counter per file path
;; ---------------------------------------------------------------------------

(define (doc-generation doc-key)
  (if (hash-contains? *wakatime-doc-generations* doc-key)
      (hash-get *wakatime-doc-generations* doc-key)
      0))

(define (bump-doc-generation! doc-key)
  (let ([next-generation (+ (doc-generation doc-key) 1)])
    (set! *wakatime-doc-generations* (hash-insert *wakatime-doc-generations* doc-key next-generation))
    next-generation))

(define (clear-doc-generation! doc-key)
  (set! *wakatime-doc-generations* (hash-remove *wakatime-doc-generations* doc-key)))

(define (clear-last-heartbeat-time! path)
  (set! *wakatime-last-heartbeat-times* (hash-remove *wakatime-last-heartbeat-times* path)))

;; ---------------------------------------------------------------------------
;; Heartbeat sending
;; ---------------------------------------------------------------------------

;; Build the argument list for wakatime-cli.
(define (wakatime-command-args path is-write)
  (append (list "--entity" path "--entity-type" "file" "--plugin" (wakatime-plugin-string))
          (if is-write
              (list "--write")
              '())))

;; Should we send a heartbeat? Write events always pass;
;; activity heartbeats are throttled to once per 2 minutes per file.
(define (should-send-heartbeat? path is-write)
  (or is-write
      (let ([last (if (hash-contains? *wakatime-last-heartbeat-times* path)
                      (hash-get *wakatime-last-heartbeat-times* path)
                      0)])
        (> (- (current-milliseconds) last) *wakatime-throttle-ms*))))

;; Send a heartbeat for doc-id on a background thread.
;; Skips untitled / empty-path documents and throttled duplicates.
(define (send-heartbeat! doc-id is-write)
  (let ([path (doc->path-string doc-id)])
    (if (not (and path (not (equal? path ""))))
        (log-warn "skipping heartbeat for untrackable document")
        (if (not (should-send-heartbeat? path is-write))
            #f
            (begin
              (set! *wakatime-last-heartbeat-times*
                    (hash-insert *wakatime-last-heartbeat-times* path (current-milliseconds)))
              (log-info
               (string-append "sending " (if is-write "write" "activity") " heartbeat for " path))
              (spawn-native-thread
               (lambda ()
                 (let ([spawned (spawn-process (command *wakatime-cli*
                                                        (wakatime-command-args path is-write)))])
                   (if (Ok? spawned)
                       (let ([status (wait (Ok->value spawned))])
                         (if (and (Ok? status) (equal? (Ok->value status) 0))
                             #t
                             (log-warn (string-append "heartbeat exited non-zero for " path))))
                       (log-warn (string-append "failed spawning wakatime-cli for " path)))))))))))

;; Schedule a debounced idle heartbeat.
;; Each call bumps the generation counter; the delayed callback only fires
;; if no further edits have occurred (i.e. generation hasn't changed).
(define (schedule-idle-heartbeat! doc-id)
  (let ([doc-key (doc->path-string doc-id)])
    (if (not (and doc-key (not (equal? doc-key ""))))
        #f
        (let ([generation (bump-doc-generation! doc-key)])
          (enqueue-thread-local-callback-with-delay
           *wakatime-idle-delay-ms*
           (lambda ()
             (let ([current-generation (doc-generation doc-key)]
                   [doc-exists (editor-doc-exists? doc-id)])
               (if (and doc-exists (= current-generation generation))
                   (send-heartbeat! doc-id #f)
                   #f))))))))

;; ---------------------------------------------------------------------------
;; Hook registration & plugin entry point
;; ---------------------------------------------------------------------------

(define (register-wakatime-hooks!)
  (register-hook 'document-opened (lambda (doc-id) (send-heartbeat! doc-id #f)))
  (register-hook 'document-saved (lambda (doc-id) (send-heartbeat! doc-id #t)))
  (register-hook 'document-focus-lost (lambda (doc-id) (send-heartbeat! doc-id #f)))
  (register-hook 'document-changed (lambda (doc-id _old-text) (schedule-idle-heartbeat! doc-id)))
  ;; No document-focus-gained hook exists; use selection-did-change as a proxy
  ;; so that switching to a file registers activity. Throttling prevents noise.
  (register-hook 'selection-did-change
                 (lambda (view-id) (send-heartbeat! (editor->doc-id view-id) #f)))
  ;; Clean up the generation counter when a document is closed
  (register-hook 'document-closed
                 (lambda (closed-event)
                   (let ([path (doc-closed-path closed-event)])
                     (if (equal? path "")
                         #f
                         (begin
                           (clear-doc-generation! path)
                           (clear-last-heartbeat-time! path)))))))

(define (install-wakatime-plugin!)
  (if *wakatime-installed*
      #f
      (begin
        (set! *wakatime-installed* #t)
        (register-wakatime-hooks!)
        (set-status! "wakatime plugin loaded"))))

(install-wakatime-plugin!)
