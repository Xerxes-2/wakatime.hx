(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin steel/core/result)
(require-builtin steel/process)
(require-builtin steel/strings)

(provide install-wakatime-plugin!)

(define *wakatime-installed* #f)
(define *wakatime-doc-generations* (hash))
(define *wakatime-cli* "wakatime-cli")
(define *wakatime-editor-name* "helix")
(define *wakatime-editor-version* #f)
(define *wakatime-plugin-name* "wakatime.hx")
(define *wakatime-plugin-version* "0.1.0")
(define *wakatime-idle-delay-ms* 2000)

(define (log-info message)
  (log::info! (string-append "[wakatime] " message)))

(define (log-warn message)
  (log::warn! (string-append "[wakatime] " message)))

(define (extract-version output)
  (let ([parts (split-many (trim output) " ")])
    (if (> (length parts) 1)
        (list-ref parts 1)
        "unknown")))

(define (editor-version)
  (if *wakatime-editor-version*
      *wakatime-editor-version*
      (begin
        (let ([commands '("hx" "helix")])
          (let loop ([remaining commands])
            (if (null? remaining)
                "unknown"
                (let ([spawned
                       (spawn-process
                         (with-stdout-piped
                           (command (car remaining) (list "--version"))))])
                  (if (Ok? spawned)
                      (let ([output (wait->stdout (Ok->value spawned))])
                        (if (Ok? output)
                            (let ([version (extract-version (Ok->value output))])
                              (set! *wakatime-editor-version* version)
                              version)
                            (loop (cdr remaining))))
                      (loop (cdr remaining))))))))))

(define (wakatime-plugin-string)
  (string-append
    *wakatime-editor-name*
    "/"
    (editor-version)
    " "
    *wakatime-plugin-name*
    "/"
    *wakatime-plugin-version*
    " "
    *wakatime-editor-name*
    "-wakatime/"
    *wakatime-plugin-version*))

(define (doc->path-string doc-id)
  (editor-document->path doc-id))

(define (doc->language-string doc-id)
  (let ([language (editor-document->language doc-id)])
    (if language language "text")))

(define (doc-trackable? doc-id)
  (let ([path (doc->path-string doc-id)])
    (and path (not (equal? path "")))))

(define (doc-generation doc-id)
  (if (hash-contains? *wakatime-doc-generations* doc-id)
      (hash-get *wakatime-doc-generations* doc-id)
      0))

(define (bump-doc-generation! doc-id)
  (let ([next-generation (+ (doc-generation doc-id) 1)])
    (set! *wakatime-doc-generations*
          (hash-insert *wakatime-doc-generations* doc-id next-generation))
    next-generation))

(define (clear-doc-generation! doc-id)
  (set! *wakatime-doc-generations*
        (hash-remove *wakatime-doc-generations* doc-id)))

(define (wakatime-command-args path language is-write)
  (append
    (list
      "--entity"
      path
      "--entity-type"
      "file"
      "--plugin"
      (wakatime-plugin-string)
      "--language"
      language)
    (if is-write
        (list "--write")
        '())))

(define (send-heartbeat! doc-id is-write)
  (if (doc-trackable? doc-id)
      (let ([path (doc->path-string doc-id)]
            [language (doc->language-string doc-id)])
        (log-info
          (string-append
            "sending "
            (if is-write "write" "activity")
            " heartbeat for "
            path))
        (spawn-native-thread
          (lambda ()
            (let ([spawned
                   (spawn-process
                     (command
                       *wakatime-cli*
                       (wakatime-command-args path language is-write)))])
              (if (Ok? spawned)
                  (let ([status (wait (Ok->value spawned))])
                    (if (and (Ok? status) (equal? (Ok->value status) 0))
                        #t
                        (log-warn
                          (string-append "heartbeat exited non-zero for " path))))
                  (log-warn
                    (string-append "failed spawning wakatime-cli for " path)))))))
      (begin
        (log-warn "skipping heartbeat for untrackable document")
        #f)))

(define (schedule-idle-heartbeat! doc-id)
  (if (not (doc-trackable? doc-id))
      #f
      (let ([generation (bump-doc-generation! doc-id)])
        (enqueue-thread-local-callback-with-delay
          *wakatime-idle-delay-ms*
          (lambda ()
            (if (and (editor-doc-exists? doc-id)
                     (= (doc-generation doc-id) generation))
                (send-heartbeat! doc-id #f)
                #f))))))

(define (register-wakatime-hooks!)
  (register-hook 'document-opened
                 (lambda (doc-id)
                   (send-heartbeat! doc-id #f)))
  (register-hook 'document-saved
                 (lambda (doc-id)
                   (send-heartbeat! doc-id #t)))
  (register-hook 'document-focus-lost
                 (lambda (doc-id)
                   (send-heartbeat! doc-id #f)))
  (register-hook 'document-changed
                 (lambda (doc-id _old-text)
                   (schedule-idle-heartbeat! doc-id)))
  (register-hook 'document-closed
                 (lambda (closed-event)
                   (clear-doc-generation! (doc-closed-id closed-event)))))

(define (install-wakatime-plugin!)
  (if *wakatime-installed*
      #f
      (begin
        (set! *wakatime-installed* #t)
        (register-wakatime-hooks!)
        (set-status! "wakatime plugin loaded"))))

(install-wakatime-plugin!)
