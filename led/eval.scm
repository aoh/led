(define-library (led eval)
   
   (export
      led-eval-command) ;; buff command-tuple → buff' 
   
   (import
      (owl base)
      (led buffer)
      (led log)
      (led undo))
   
   (begin
    
      (define (maybe-car exp default)
         (if (pair? exp)
            (car exp) 
            default))
      
      (define (largs sexp)
         (if (= (length sexp) 3)
            (values (cadr sexp) (caddr sexp))
            (values #f #f)))
     
      ;; todo: allow arithmetic 
      (define (eval-position buff pos)
         (cond
            ((number? pos) 
               (cond
                  ((< pos 0)
                     (max 1 (+ (buffer-current-line buff) pos)))
                  ((= pos 0)
                     ;; first line is 1
                     1)
                  (else pos)))
            ((eq? pos 'dot) 
               (buffer-current-line buff))
            ((eq? pos 'end) 
               (buffer-line-count buff))
            (else 
               (log "ERROR: interpret-position " pos)
               #false)))

      ;; bytes path → bool
      (define (bytes->file bytes path)
         (let ((port (open-output-file path)))
            (if port
               (let ((outcome (write-bytes port bytes)))
                  (log "write -> " outcome)
                  (close-port port)
                  outcome)
               #false)))
      
      (define (led-eval-command buff undo command)
         (let ((op (maybe-car command #false)))
            (cond
               ((or (eq? op 'write) (eq? op 'write!))
                  (lets ((range path (largs command)))
                     (if (equal? range '(interval 1 end))
                        ;; full write also marks the buffer as saved
                        (lets ((ok? msg (write-buffer buff path)))
                           (if ok?
                              (values 
                                 (put-buffer-meta buff 'path path)
                                 (mark-saved undo buff (time-ms))
                                 "saved")
                              (values buff undo 
                                 (or msg "save failed"))))
                        (lets
                           ((from to (largs range))
                            (from (eval-position buff from))
                            (to (eval-position buff to)))
                           (if (and from to (<= from to))
                              (let ((data (buffer-range->bytes buff from to)))
                                 (values buff undo
                                    (if (bytes->file data path)
                                       "saved range"
                                       "failed to save range")))
                              (values buff undo "bad range"))))))
               (else
                  (values buff undo "led-eval is confused")))))))
                  

