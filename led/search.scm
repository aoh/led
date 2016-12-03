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

      (define search-timeout 10)
      
      (define (match-prefix? data pfx)
         (cond
            ((null? pfx) #true)
            ((null? data) #false)
            ((eq? (car pfx) (car data))
               (match-prefix? (cdr data) (cdr pfx)))
            (else #false)))
            
      (define (file-results bs path data end tail)
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
                  (if (> (time) end)
                     tail
                     (loop (cdr data) 1 (+ y 1) null tail)))
               (else
                  (loop (cdr data) (+ x 1) y (cons (car data) l) tail)))))
         
      (define (search-results what where log ok? searched end)
         (if (not (null? where))
            (log (str "Considering '" (car where) "'")))
         (cond
            ((null? where)
               null)
            ((mem equal? searched (car where))
               (search-results what (cdr where) log ok? searched end))
            ((> (time) end)
               (list "" "SEARCH TIMEOUT" "Search took too long. Maybe you want to remove some less relevant directories from buffer 1?"))
            ((led-dir->list (car where)) =>
               (lambda (paths)
                  (log "Including contents")
                  (search-results what
                     (append
                        (map (lambda (x) (str (car where) "/" x)) paths)
                        (cdr where))
                     log ok? (cons (car where) searched) end)))
            ((not (ok? (car where)))
               (log (str "Skipping '" (car where) "'"))
               (search-results what (cdr where) log ok? searched end))
            ((file->list (car where)) =>
               (lambda (data)
                  (log (str "Searching from '" (car where) "'"))
                  (file-results what (car where) data end
                     (search-results what (cdr where) log ok? (cons (car where) searched) end))))
            (else
               (search-results what (cdr where) log ok? searched end))))
            
      (define (run-search what where log path-ok?)
         (map string->list
            (ilist
               (str "Searched for '" what "'")
               ""
               (reverse
                  (search-results (string->bytes what) (reverse where) log path-ok? null
                     (+ (time) search-timeout))))))))
            
