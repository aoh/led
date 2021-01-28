(define-library (led log)

   (import (owl toplevel))

   (export
      log start-logger)

   (begin

      (define (log . x)
         (mail 'log x))

      (define (logger port)
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (print-to port from ": " msg)
            (logger port)))

      (define (unlogger)
         (thread 'log
            (let loop ()
               (wait-mail)
               (loop))))

      (define (start-logger path)
         (print-to stderr "GC: " path)
         (if path
            (let ((port (open-output-file path)))
               (if port
                  (begin
                     (thread 'log (logger port))
                     log)
                  (error "Cannot open log file " path)))
            (unlogger)))))
