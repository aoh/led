(define-library (led log)
   (import (owl base))
      
   (export 
      log start-logger)
      
   (begin
      
      (define (log . x)
         ;; just dropped if logger is not running
         (mail 'log x))
      
      (define (logger port)
         (lets ((envelope (wait-mail))
                (from msg envelope))
            (print-to port from ": " msg)
            (logger port)))
      
      (define (start-logger path)
         (if path
            (let ((port (open-output-file path)))
               (if port
                  (begin
                     (thread 'log (logger port))
                     log)
                  (error "Cannot open log file " path)))))
                  ))
