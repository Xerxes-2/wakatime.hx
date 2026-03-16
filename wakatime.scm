(require "helix/editor.scm")
(require "helix/misc.scm")

(require-builtin steel/core/result)
(require-builtin steel/process)
(require-builtin steel/strings)

(provide install-wakatime-plugin!)

(define *wakatime-installed* #f)
(define *wakatime-pending-docs* (hash))
(define *wakatime-cli* "wakatime-cli")
(define *wakatime-editor-name* "helix")
(define *wakatime-editor-version* #f)
(define *wakatime-plugin-name* "helix-steel-wakatime")
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
        (let ([spawned (spawn-process
                         (with-stdout-piped (command "hx" (list "--version"))))])
          (if (Ok? spawned)
              (let ([output (wait->stdout (Ok->value spawned))])
                (if (Ok? output)
                    (let ([version (extract-version (Ok->value output))])
                      (set! *wakatime-editor-version* version)
                      version)
                    "unknown"))
              "unknown")))))

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

(define (pending-doc? doc-id)
  (and (hash-contains? *wakatime-pending-docs* doc-id)
       (hash-get *wakatime-pending-docs* doc-id)))

(define (set-pending-doc! doc-id value)
  (set! *wakatime-pending-docs*
        (hash-insert *wakatime-pending-docs* doc-id value)))

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
  (if (or (not (doc-trackable? doc-id)) (pending-doc? doc-id))
      #f
      (begin
        (set-pending-doc! doc-id #t)
        (enqueue-thread-local-callback-with-delay
          *wakatime-idle-delay-ms*
          (lambda ()
            (set-pending-doc! doc-id #f)
            (if (editor-doc-exists? doc-id)
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
                   (schedule-idle-heartbeat! doc-id))))

(define (install-wakatime-plugin!)
  (if *wakatime-installed*
      #f
      (begin
        (set! *wakatime-installed* #t)
        (register-wakatime-hooks!)
        (set-status! "wakatime plugin loaded"))))

(install-wakatime-plugin!)
