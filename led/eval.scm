(define-library (led eval)
   
   (export
      led-eval-command) ;; buff command-tuple → buff' 
   
   (import
      (led buffer)
      (led undo)
      (owl base))
   
   (begin
    
      (define (maybe-car exp default)
         (if (pair? exp)
            (car exp) 
            default))
      
      (define (largs sexp)
         (if (= (length sexp) 3)
            (values (cadr sexp) (caddr sexp))
            (values #f #f)))
         
      (define (led-eval-command buff undo command)
         (let ((op (maybe-car command #false)))
            (cond
               ((eq? op 'write)
                  (lets ((range path (largs command)))
                     (if (equal? range '(interval 1 end))
                        (lets ((ok? msg (write-buffer buff path)))
                           (if ok?
                              (values 
                                 (put-buffer-meta buff 'path path)
                                 (mark-saved undo buff (time-ms))
                                 "saved")
                              (values buff undo 
                                 (or msg "save failed"))))
                        (values buff undo "partial write not supported yet"))))
               (else
                  (values buff undo "led-eval is confused")))))))
                  

