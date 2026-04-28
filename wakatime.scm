;;; wakatime.scm — WakaTime plugin for Helix
;;;
;;; Tracks coding activity by sending heartbeats to wakatime-cli on
;;; document open, save, focus-lost, and idle-after-edit events.
;;; Idle heartbeats use a generation-based debounce: each edit bumps a
;;; per-file counter, and the delayed callback only fires if the counter
;;; hasn't changed (i.e. no further edits arrived within the delay window).

(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "steel/result")

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
;; cached after the first detection attempt
(define *wakatime-editor-version* #f)
;; cached after first build so the string isn't rebuilt per heartbeat
(define *wakatime-plugin-string* #f)
(define *wakatime-plugin-name* "wakatime-hx")
(define *wakatime-plugin-version* "0.1.0")
(define *wakatime-idle-delay-ms* 2000)
;; Throttle: skip repeat activity heartbeats for the same file within this window.
;; Write events always go through regardless.
(define *wakatime-throttle-seconds* 120)
;; path -> last heartbeat instant
(define *wakatime-last-heartbeat-instants* (hash))
;; Last document observed through selection changes. Used to avoid treating every
;; cursor movement as focus activity.
(define *wakatime-last-active-doc-id* #f)

;; ---------------------------------------------------------------------------
;; Logging helpers
;; ---------------------------------------------------------------------------

(define (log-info! message)
  (log::info! (string-append "[wakatime] " message)))

(define (log-warn! message)
  (log::warn! (string-append "[wakatime] " message)))

(define (warn-untrackable-document!)
  (log-warn! "skipping heartbeat for untrackable document"))

(define (warn-spawn-failed! path)
  (log-warn! (string-append "failed spawning wakatime-cli for " path)))

(define (warn-non-zero-exit! path)
  (log-warn! (string-append "heartbeat exited non-zero for " path)))

;; ---------------------------------------------------------------------------
;; Generic helpers
;; ---------------------------------------------------------------------------

;; Convert exception style error to Result
(define-syntax try-result
  (syntax-rules ()
    [(try-result body ...)
     (call/cc (lambda (k)
                (call-with-exception-handler (lambda (exn) (k (Err exn)))
                                             (lambda ()
                                               (Ok (begin
                                                     body ...))))))]))

(define (hash-get/default table key default)
  (or (hash-try-get table key) default))

;; ---------------------------------------------------------------------------
;; Editor version detection
;; ---------------------------------------------------------------------------

;; Parse "helix 24.7" → "24.7"
(define (extract-version output)
  (~> output trim split-whitespace second try-result))

;; single command
(define (detect-version-with command-name)
  (~> (command command-name (list "--version"))
      with-stdout-piped
      spawn-process
      (ok-and-then wait->stdout)
      (ok-and-then extract-version)))

;; list of possible commands
(define (detect-editor-version commands)
  (if (null? commands)
      "unknown"
      (unwrap-or (detect-version-with (car commands)) (detect-editor-version (cdr commands)))))

;; Try "hx --version", then "helix --version"; cache the result.
(define (editor-version!)
  (or *wakatime-editor-version*
      (let ([version (detect-editor-version '("hx" "helix"))])
        (set! *wakatime-editor-version* version)
        version)))

;; Build the --plugin flag value, e.g. "helix/24.7 wakatime-hx/0.1.0".
;; Cached after first call.
(define (wakatime-plugin-string!)
  (or *wakatime-plugin-string*
      (let ([s (string-append *wakatime-editor-name*
                              "/"
                              (editor-version!)
                              " "
                              *wakatime-plugin-name*
                              "/"
                              *wakatime-plugin-version*)])
        (set! *wakatime-plugin-string* s)
        s)))

;; ---------------------------------------------------------------------------
;; Document helpers
;; ---------------------------------------------------------------------------

(define (doc->path-string doc-id)
  (editor-document->path doc-id))

(define (trackable-path? path)
  (and path (not (equal? path ""))))

;; ---------------------------------------------------------------------------
;; Document state
;; ---------------------------------------------------------------------------

(define (doc-generation doc-key)
  (hash-get/default *wakatime-doc-generations* doc-key 0))

(define (bump-doc-generation! doc-key)
  (let ([next-generation (+ (doc-generation doc-key) 1)])
    (set! *wakatime-doc-generations* (hash-insert *wakatime-doc-generations* doc-key next-generation))
    next-generation))

(define (clear-doc-generation! doc-key)
  (set! *wakatime-doc-generations* (hash-remove *wakatime-doc-generations* doc-key)))

(define (clear-active-doc!)
  (set! *wakatime-last-active-doc-id* #f))

;; ---------------------------------------------------------------------------
;; Activity throttling
;; ---------------------------------------------------------------------------

(define (last-heartbeat-instant path)
  (hash-get/default *wakatime-last-heartbeat-instants* path #f))

(define (record-heartbeat-instant! path)
  (set! *wakatime-last-heartbeat-instants*
        (hash-insert *wakatime-last-heartbeat-instants* path (instant/now))))

(define (clear-last-heartbeat-instant! path)
  (set! *wakatime-last-heartbeat-instants* (hash-remove *wakatime-last-heartbeat-instants* path)))

(define (clear-document-state! path)
  (clear-doc-generation! path)
  (clear-last-heartbeat-instant! path))

(define (duration-since-in-seconds instant)
  (duration->seconds (duration-since (instant/now) instant)))

(define (throttle-expired? last-instant)
  (or (not last-instant) (>= (duration-since-in-seconds last-instant) *wakatime-throttle-seconds*)))

;; Write events always pass; activity heartbeats are throttled per file.
(define (should-send-heartbeat? path is-write)
  (or is-write (throttle-expired? (last-heartbeat-instant path))))

;; ---------------------------------------------------------------------------
;; Heartbeat sending
;; ---------------------------------------------------------------------------

(define (heartbeat-kind is-write)
  (if is-write "write" "activity"))

(define (log-heartbeat-start! path is-write)
  (log-info! (string-append "sending " (heartbeat-kind is-write) " heartbeat for " path)))

;; Build the argument list for wakatime-cli.
(define (wakatime-command-args path is-write lineno cursorpos)
  (append (list "--entity" path "--entity-type" "file" "--plugin" (wakatime-plugin-string!))
          (if is-write
              (list "--write")
              '())
          (if lineno
              (list "--lineno" (int->string lineno))
              '())
          (if cursorpos
              (list "--cursorpos" (int->string cursorpos))
              '())))

(define (wait-for-heartbeat! process path)
  (let ([status (wait process)])
    (unless (equal? status (Ok 0))
      (warn-non-zero-exit! path))))

(define (run-wakatime-cli! path is-write lineno cursorpos)
  (~> (wakatime-command-args path is-write lineno cursorpos)
      (command *wakatime-cli*)
      spawn-process
      (ok-and-then (lambda (proc) (wait-for-heartbeat! proc path)))
      (unwrap-or (warn-spawn-failed! path))))

(define (spawn-heartbeat-thread! path is-write lineno cursorpos)
  (spawn-native-thread (lambda () (run-wakatime-cli! path is-write lineno cursorpos))))

(define (send-heartbeat-for-path! path is-write lineno cursorpos)
  (cond
    [(not (trackable-path? path)) (warn-untrackable-document!)]
    [(not (should-send-heartbeat? path is-write)) #f]
    [else
     (record-heartbeat-instant! path)
     (log-heartbeat-start! path is-write)
     (spawn-heartbeat-thread! path is-write lineno cursorpos)]))

;; Get current cursor info (lineno . cursorpos), or #f for each if unavailable.
(define (current-cursor-info)
  (let ([lineno (try-result (get-current-line-number))]
        [cursorpos (try-result (cursor-position))])
    (cons (unwrap-or lineno #f) (unwrap-or cursorpos #f))))

;; Send a heartbeat for doc-id on a background thread.
;; Skips untitled / empty-path documents and throttled duplicates.
(define (send-heartbeat! doc-id is-write)
  (let ([info (current-cursor-info)])
    (send-heartbeat-for-path! (doc->path-string doc-id) is-write (car info) (cdr info))))

(define (send-activity-heartbeat! doc-id)
  (send-heartbeat! doc-id #f))

(define (send-write-heartbeat! doc-id)
  (send-heartbeat! doc-id #t))

;; ---------------------------------------------------------------------------
;; Idle heartbeat debounce
;; ---------------------------------------------------------------------------

(define (idle-heartbeat-current? doc-id path generation)
  (and (editor-doc-exists? doc-id) (= (doc-generation path) generation)))

(define (send-idle-heartbeat-if-current! doc-id path generation)
  (when (idle-heartbeat-current? doc-id path generation)
    (send-activity-heartbeat! doc-id)))

(define (schedule-idle-heartbeat-for-path! doc-id path)
  (let ([generation (bump-doc-generation! path)])
    (enqueue-thread-local-callback-with-delay
     *wakatime-idle-delay-ms*
     (lambda () (send-idle-heartbeat-if-current! doc-id path generation)))))

;; Schedule a debounced idle heartbeat.
;; Each call bumps the generation counter; the delayed callback only fires
;; if no further edits have occurred (i.e. generation hasn't changed).
(define (schedule-idle-heartbeat! doc-id)
  (let ([path (doc->path-string doc-id)])
    (when (trackable-path? path)
      (schedule-idle-heartbeat-for-path! doc-id path))))

;; ---------------------------------------------------------------------------
;; Hook handlers
;; ---------------------------------------------------------------------------

(define (handle-document-changed! doc-id _old-text)
  (schedule-idle-heartbeat! doc-id))

(define (handle-selection-did-change! view-id)
  (let ([doc-id (editor->doc-id view-id)])
    (when (and doc-id (not (equal? doc-id *wakatime-last-active-doc-id*)))
      (set! *wakatime-last-active-doc-id* doc-id)
      (send-activity-heartbeat! doc-id))))

(define (handle-document-closed! closed-event)
  (clear-active-doc!)
  (let ([path (doc-closed-path closed-event)])
    (when (trackable-path? path)
      (clear-document-state! path))))

;; ---------------------------------------------------------------------------
;; Hook registration & plugin entry point
;; ---------------------------------------------------------------------------

(define (register-wakatime-hooks!)
  (register-hook 'document-opened send-activity-heartbeat!)
  (register-hook 'document-saved send-write-heartbeat!)
  (register-hook 'document-focus-lost send-activity-heartbeat!)
  (register-hook 'document-changed handle-document-changed!)
  ;; No document-focus-gained hook exists; use selection-did-change as a proxy.
  ;; Only document switches register activity; cursor movement is ignored here.
  (register-hook 'selection-did-change handle-selection-did-change!)
  ;; Clean up the generation counter when a document is closed
  (register-hook 'document-closed handle-document-closed!))

(define (install-wakatime-plugin!)
  (unless *wakatime-installed*
    (set! *wakatime-installed* #t)
    (wakatime-plugin-string!)
    (register-wakatime-hooks!)
    (set-status! "wakatime plugin loaded")))

(install-wakatime-plugin!)
