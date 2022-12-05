(define-library (led find)

   (import
      (owl toplevel)
      (led system)
      (led log)
      (only (owl unicode) utf8-decode)
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

      ; -> #(push (byte ...))
      (define (match-finder data row-start path pat id row tl)
         (cond
            ((null? data)
               tl)
            ((match-here? data pat)
               (lets
                  ((row-chars (grab-row row-start))
                   (row-string (str path ":" row ":" (list->string row-chars) "\n")))
                  (cons row-string
                     (lambda ()
                        (match-finder (cdr data) row-start path pat id row tl)))))
            ((eq? (car data) #\newline)
               (match-finder (cdr data) (cdr data) path pat id (+ row 1) tl))
            (else
               (match-finder (cdr data) row-start path pat id row tl))))

      (define send-interval-ms 500)

      (define (flush id rlines)
         (mail id
            (tuple 'push
               (foldr
                  (lambda (s tl)
                     (append (string->list s) tl))
                  '()
                  rlines))))

      (define (result-sender ress id at rlines)
         (cond
            ((null? ress)
               (flush id rlines))
            ((pair? ress)
               (let ((rlines (cons (car ress) rlines))
                     (now (time-ms)))
                  (if (> now at)
                     (begin
                        (flush id rlines)
                        (result-sender (cdr ress) id (+ now send-interval-ms) '()))
                     (result-sender (cdr ress) id at rlines)))
               )
            (else
               (result-sender (ress) id at rlines))))

      (define (start-find-thread env chars id)
         (thread (list 'finder-of id)
            (begin
               (mail id
                  (tuple 'set-status-text "Search running..."))
               (result-sender
                  (lets
                     ((path-ll (path-finder env (get env 'find-path ".") '()))
                      (path-ll (lkeep (get env 'find-regex) path-ll)))
                     (lfold
                        (lambda (tl path)
                           (lets
                               ((data (file->list path))            ;; <- streaming would reduce memory load
                                (data (utf8-decode (or data '()))))
                              (if data
                                 (match-finder data data path chars id 1 tl)
                                 tl)))
                        '()
                        path-ll))
                  id (+ (time-ms) 100) '())
               (mail id
                  (tuple 'set-status-text "Search finished."))
               )))))
