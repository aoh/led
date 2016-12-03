(define-library (led search)
   
   (export
      run-search ;; what-str maybe-paths -> ((node ...) ...)
      )
   
   (import
      (owl base)
      (owl sys)
      (led log)
      (led system))
   
   (begin

      (define (match-prefix? data pfx)
         (cond
            ((null? pfx) #true)
            ((null? data) #false)
            ((eq? (car pfx) (car data))
               (match-prefix? (cdr data) (cdr pfx)))
            (else #false)))
            
      (define (file-results bs path data tail)
         (let loop ((data data) (x 1) (y 1) (l null) (tail tail))
            (cond
               ((null? data)
                  tail)
               ((match-prefix? data bs)
                  (lets
                     ((r _ (take-while (lambda (x) (not (eq? x 10))) data))
                      (line (bytes->string (append (reverse l) r))))
                     (loop (cdr data) (+ x 1) y
                        (cons (car data) l)
                        (cons 
                           (str path "+" y "." x ": " line)
                           tail))))
               ((eq? 10 (car data))
                  (loop (cdr data) 1 (+ y 1) null tail))
               (else
                  (loop (cdr data) (+ x 1) y (cons (car data) l) tail)))))
         
      (define (search-results what where log ok? searched)
         (if (not (null? where))
            (log (str "Searching from '" (car where) "'")))
         (cond
            ((null? where)
               null)
            ((mem equal? searched (car where))
               (search-results what (cdr where) log ok? searched))
            ((led-dir->list (car where)) =>
               (lambda (paths)
                  (log "Including contents")
                  (search-results what
                     (append
                        (map (lambda (x) (str (car where) "/" x)) paths)
                        (cdr where))
                     log ok? (cons (car where) searched))))
            ((not (ok? (car where)))
               (search-results what (cdr where) log ok? searched))
            ((file->list (car where)) =>
               (lambda (data)
                  (file-results what (car where) data
                     (search-results what (cdr where) log ok? (cons (car where) searched)))))
            (else
               (search-results what (cdr where) log ok? searched))))
            
      (define (run-search what where log path-ok?)
         (map string->list
            (cons 
               (str "Searched for '" what "'")
               (reverse
                  (search-results (string->bytes what) where log path-ok? null)))))))
            
