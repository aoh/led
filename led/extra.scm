;;;
;;; Extra processing functions
;;;

(define-library (led extra)

   ;; there are intentionally no led-specific imports

   (import
      (owl base))

   (export
      extra-functions
      find-extra)

   (begin

      (define null '())

      (define (lex-less? a b)
         (cond
            ((null? a) #true)
            ((null? b) #false)
            ((< (car a) (car b)) #true)
            ((= (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #false)))

      (define (lex-sort lines)
         (sort lex-less? lines))

      (define (format lines)
         (list
            (foldr append '() lines)))

      ;;; paragraph formatting

      (define (pick-line data len)
         (cond
            ((null? data)
               (values null null))
            ((eq? len 0)
               (values #f #f))
            ((eq? (car data) #\space)
               (lets ((this rest (pick-line (cdr data) (- len 1))))
                  (if this
                     (values
                        (cons #\space this)
                        rest)
                     (values null (cdr data)))))
            (else
               (lets ((this rest (pick-line (cdr data) (- len 1))))
                  (if this
                     (values (cons (car data) this) rest)
                     (values #f #f))))))

      (define (format-lines data)
         (if (null? data)
            null
            (lets ((this rest (pick-line data 80)))
               (if this
                  (cons this (format-lines rest))
                  (list data)))))

      (define (format-merged lines)
         (format-lines
            (s/  */ /g
               (foldr append null lines))))

      (define extra-functions
         (list
            (cons "sort" lex-sort)
            (cons "fmt" format-merged)
            ))

      (define (find-extra name)
         (let ((res (assoc name extra-functions)))
            (if (pair? res)
               (cdr res)
               #false)))
))



