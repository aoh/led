(define-library (led eval)
   
   (export
      led-eval-command) ;; buff command-tuple → buff' 
   
   (import
      (owl base)
      (led buffer)
      (only (led node) nodes->code-points)
      (led ops)
      (led log)
      (led extra)
      (led undo))
   
   (begin
    
      (define (maybe-car exp default)
         (if (pair? exp)
            (car exp) 
            default))
    
      ;; unary/binary temporary helper
      (define (largs sexp)
         (cond
            ((= (length sexp) 3)
               (values (cadr sexp) (caddr sexp)))
            ((= (length sexp) 2)
               (values (cadr sexp) #false))
            (else
               (values #f #f))))

      ;; buff label-char → line | #false
      (define (label-line buff mark)
         (let ((pos (get (get-buffer-meta buff 'marks #empty) mark #false)))
            (if pos
               (+ 1 (cdr pos)) ;; absolute offset → line number
               #false)))         
     
      (define (apply-position buff op a b) 
         (cond
            ;; unary ones
            ((not a) #false)
            ((eq? op 'label)
               (label-line buff a))
            ;; binary ones
            ((not b) #false)
            ((eq? op '+) (+ a b))
            ((eq? op '-) (- a b))
            (else 
               (log "ERROR: apply-position: " op)
               #false)))
     
      (define (eval-position buff pos)
         (cond
            ((not pos) pos)
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
            ((and (pair? pos) (list? pos))
               (lets ((a b (largs pos)))
                  (apply-position buff (car pos)
                     (eval-position buff a)
                     (eval-position buff b))))
            (else 
               (log "ERROR: interpret-position " pos)
               #false)))

      ;; position-or-interval → start|#false end|#false
      (define (eval-interval buff pos)
         (if (eq? (maybe-car pos #f) 'interval)
            (lets ((from to (largs pos)))
               (values
                  (eval-position buff from)
                  (eval-position buff to)))
            (let ((res (eval-position buff pos)))
               (values res res))))
      
      ;; bytes path → bool
      (define (bytes->file bytes path)
         (let ((port (open-output-file path)))
            (if port
               (let ((outcome (write-bytes port bytes)))
                  (close-port port)
                  outcome)
               #false)))
     
      ;; todo: add an env to avoid this blowing up 
      (define (led-eval-command buff undo command)
         (log "eval: " command)
         (let ((op (maybe-car command #false)))
            (cond
               ;; fixme: write and write! are treated equally
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
               ((eq? op 'delete)
                  (lets ((range to-buffer (largs command))
                         (from to (eval-interval buff range)))
                      (if (and from to (<= from to))
                         (values 
                            (op-delete-lines buff from to to-buffer)
                            (push-undo undo buff)
                            "deleted")
                         (values buff undo "bad range"))))
               ((eq? op 'put)
                  (lets ((where reg (largs command))
                         (where (eval-position buff where))
                         (buffp (op-paste-register buff where reg)))
                     (if (eq? buff buffp)
                        (values buff undo #false)
                        (values buffp 
                           (push-undo undo buff)
                           "pasted"))))
               ((eq? op 'lisp)
                  (lets ((range name (largs command))
                         (from to (eval-interval buff range))
                         (func (find-extra name))
                         (buffp (op-delete-lines buff from to 'lisp)))
                     (cond
                        ((eq? buff buffp)
                           ;; could not cut the range
                           (values buff undo "bad range"))
                        ((not func)
                           (values buff undo "unknown extra"))
                        (else
                           (lets 
                              ((node (get-copy-buffer buffp 'lisp (tuple 'lines null)))
                               (lines (map nodes->code-points (ref node 2)))
                               (data (func lines))
                               (buffp (put-copy-buffer buffp 'lisp-result (tuple 'lines data))))
                              (values
                                 (op-paste-register buffp (- from 1) 'lisp-result)
                                 (push-undo undo buff)
                                 "evaluated"))))))
               (else
                  (values buff undo "led-eval is confused")))))))
                  

