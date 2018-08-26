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

      (define null '())
            
      (define search-timeout 10) ;; timeout after which search is stopped

      (define max-file-results 1000) ;; max results per file (protect against result set blowup)

      (define max-result-line-length 500) ;; match preview line maximum length

      (define (match-prefix? data pfx)
         (cond
            ((null? pfx) #true)
            ((null? data) #false)
            ((eq? (car pfx) (car data))
               (match-prefix? (cdr data) (cdr pfx)))
            (else #false)))

      (define (safe-char x)
         (cond
            ((< x 32) #\.)
            ((> x 126) #\.)
            (else x)))

      (define (file-results bs path data end tail)
         (let loop ((data data) (x 1) (y 1) (n max-file-results) (l null) (tail tail))
            (cond
               ((null? data)
                  tail)
               ((eq? n 0)
                  (cons
                     (str "(stopping after " max-file-results " hits)")
                      tail))
               ((match-prefix? data bs)
                  (lets
                     ((r _ (take-while (λ (x) (not (eq? x 10))) data))
                      (line (bytes->string (map safe-char (take (append (reverse l) r) max-result-line-length)))))
                     (loop (cdr data) (+ x 1) y (- n 1)
                        (cons (car data) l)
                        (cons
                           (str path ":" y ":" x ": " line)
                           tail))))
               ((eq? 10 (car data))
                  (if (> (time) end)
                     tail
                     (loop (cdr data) 1 (+ y 1) n null tail)))
               (else
                  (loop (cdr data) (+ x 1) y n (cons (car data) l) tail)))))

      (define (mem op lst x)
         (cond
	    ((null? lst) #f)
	    ((op (car lst) x) #t)
	    (else (mem op (cdr lst) x))))
	

      (define (search-results what where status ok? searched end)
         (if (not (null? where))
            (log (str "search '" (car where) "'")))
         (cond
            ((null? where)
               null)
            ((mem equal? searched (car where))
               (search-results what (cdr where) status ok? searched end))
            ((> (time) end)
               (list "" "SEARCH TIMEOUT" "Search took too long. Maybe you want to remove some less relevant directories from buffer 1?"))
            ((led-dir->list (car where)) =>
               (λ (paths)
                  (log (str (car where) " has " paths))
                  (status (str (car where) "..."))
                  (search-results what
                     (append paths (cdr where))
                     status ok? (cons (car where) searched) end)))
            ((not (ok? (car where)))
               (status (str "Skipping '" (car where) "'"))
               (search-results what (cdr where) status ok? searched end))
            ((file->list (car where)) =>
               (λ (data)
                  (status (car where))
                  ; (status (str "Searching from '" (car where) "'"))
                  (file-results what (car where) data end
                     (λ ()
                        (search-results what (cdr where) status ok? (cons (car where) searched) end)))))
            (else
               (search-results what (cdr where) status ok? searched end))))

      (define (run-search what where log path-ok?)
         (if (equal? what "")
            (list (string->list "Nothing to search"))
            (map string->list
               (ilist
                  (str "Searched for '" what "'")
                  ""
                  (reverse
                     (force-ll
                        (search-results (string->bytes what) (reverse where) log path-ok? null
                           (+ (time) search-timeout))))))))))


