(define-library (led undo)
   
   (export
      initial-undo
      empty-undo
      push-undo
      pop-undo
      mark-saved 
      dirty-buffer?
      unpop-undo)
      
   (import
      (owl base)
      (led buffer)
      (led log))
   
   (begin

      (define empty-undo 
         (cons null null))

      (define (initial-undo buff)
         (let ((type (get-buffer-meta buff 'type 'file)))
            (if (eq? type 'file)
               (cons (list (cons (time-ms) buff)) null) ;; initially saved
               (cons (list buff) null))))
     
      ;; undo node = buffer | (save-timestamp . buffer)
       
      ;; fixme - should have current at head of undo for dirtiness, or keep track otherwise
      (define (push-undo undo buff)
         (log "pushing new version")
         (lets ((prev new undo))
            ;; no way to return to future after changing the past
            (cons (cons buff prev) null)))
    
      (define (unsaved x)
         (if (pair? x) (cdr x) x))
       
      (define (mark-saved undo buff when)
         (lets ((prev new undo))
            (if (eq? prev buff)
               (mark-saved undo (cons prev new) when)
               (cons 
                  (cons (cons when buff) (map unsaved prev))
                   null))))
      
      (define (dirty-buffer? buff undo)
         (not (pair? (caar undo))))
         
      (define (pop-undo undo buff)
         (log "popping undo")
         (lets ((prev new undo))
            (if (null? prev)
               (values undo buff)
               (values 
                  (cons (cdr prev) (cons buff new)) 
                  (unsaved (car prev))))))
      
      (define (unpop-undo undo buff)
         (log "unpopping undo")
         (lets ((prev new undo))
            (if (null? new)
               (values undo buff)
               (values (cons (cons buff prev) (cdr new))
                  (unsaved (car new))))))))
