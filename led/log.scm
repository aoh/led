(define-library (led log)

   (export
      start-log ;; cmdline-dict -> logger thread, or terminate everything
      start-recorder
      log)

   (import
      (owl base))

   (begin

      (define (sink arg)
         (sink (wait-mail)))

      (define (log-to port)
         (print-to port (ref (wait-mail) 2))
         (log-to port))

      (define (record-to port)
         (write-bytes port (list (ref (wait-mail) 2)))
         (record-to port))

      (define (start-log meta)
         (let ((log-path (get meta 'log)))
            (if log-path
               (let ((port (open-output-file log-path)))
                  (if port
                     (begin
                        (print-to port "Started logging")
                        (log-to port))
                     (begin
                        (print-to stderr "could not open log file " log-path)
                        (car 'logger-error)))) ;; for now, just terminate led via error
               (sink #f))))

      (define (start-recorder meta)
         (let ((rec-path (get meta 'record)))
            (if rec-path
               (let ((port (open-output-file rec-path)))
                  (if port
                     (record-to port)))
               (sink #f))))

      (define (log . what)
         (mail 'logger what)
         ; (wait 10) ;; wait for message to be logged in case of crash
         )))
