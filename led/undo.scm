(define-library (led undo)
   
   (export
      initial-undo
      push-undo
      pop-undo
      unpop-undo)
      
   (import
      (owl base)
      (led log))
   
   (begin
      (define (initial-undo buff)
         (cons (list buff) null))
      
      ;; fixme - should have current at head of undo for dirtiness, or keep track otherwise
      (define (push-undo undo buff)
         (log "pushing new version")
         (lets ((prev new undo))
            ;; no way to return to future after changing the past
            (cons (cons buff prev) null)))
      
      (define (pop-undo undo buff)
         (log "popping undo")
         (lets ((prev new undo))
            (if (null? prev)
               (values undo buff)
               (values
                  (cons (cdr prev) (cons buff new))
                  (car prev)))))
      
      (define (unpop-undo undo buff)
         (log "unpopping undo")
         (lets ((prev new undo))
            (if (null? new)
               (values undo buff)
               (values (cons (cons buff prev) (cdr new)) (car new)))))))