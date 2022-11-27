(define-library (led find)

   (import
      (owl toplevel)
      (led system)
      (only (owl sys) file?))

   (export
      start-find-thread)

   (begin
      (define (path-finder env full-path tail)
         (cond
            ((file? full-path)
               (cons full-path tail))
            ((led-dir->list full-path) =>
               (lambda (nodes)
                  (lambda ()
                     (foldr
                        (lambda (x tail)
                           (path-finder env
                              x
                              tail))
                        tail nodes))))
            (else tail)))

      (define (match-here? data pat)
         (cond
            ((null? pat) #t)
            ((null? data) #f)
            ((eq? (car data) (car pat))
               (match-here? (cdr data) (cdr pat)))
            (else #f)))

      (define (grab-row lst)
         (if (or (null? lst) (eq? (car lst) #\newline))
            '()
            (cons (car lst)
               (grab-row (cdr lst)))))

      (define (match-finder data row-start path pat id row)
         (cond
            ((null? data)
               'ok)
            ((match-here? data pat)
               (let ((row-chars (grab-row row-start)))
                  (mail id (tuple 'push (string->list (str path ":" row ":" (list->string row-chars) "\n"))))
                  (sleep 100)  ;; not really needed: give time for the receiving buffer to work
                  (match-finder (cdr data) row-start path pat id row)))
            ((eq? (car data) #\newline)
               (match-finder (cdr data) (cdr data) path pat id (+ row 1)))
            (else
               (match-finder (cdr data) row-start path pat id row))))

      (define (start-find-thread env chars id)
         (thread (list 'finder-of id)
            (begin
               (mail id
                  (tuple 'set-status-text "Search running..."))
               (lfold
                  (lambda (_ path)
                     (if ((get env 'find-regex) path)
                        (let ((data (file->list path)))
                           (if data
                              (match-finder data data path chars id 1)))))
                  42 (path-finder env (get env 'find-path ".") '()))
               (mail id
                  (tuple 'set-status-text "Search finished."))
               )))))
