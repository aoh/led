;;;
;;; Extra processing functions, TODO convert
;;;

(define-library (led extra)

   ;; there are intentionally no led-specific imports

   (import
      (owl base)
      (owl date))

   (export
      extra-functions
      find-extra
      format-merged
      format-lines
      )

   (begin

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

      (define (drop-leading-space data)
         (if (and (pair? data) (eq? (car data) #\space))
            (drop-leading-space (cdr data))
            data))

      (define (format-lines data)
         (if (null? data)
            null
            (lets
               ((data (drop-leading-space data))
                (this rest (pick-line data 80)))
               (if this
                  (cons this (format-lines rest))
                  (list data)))))

      (define (format-merged settings lines)
         (format-lines
            (s/  */ /g ;; remove duplicate spaces
               (fold ;; join by spaces
                  (λ (x tl)
                     (if (null? tl) x (append x (cons #\space tl))))
                  null lines))))

      (define (current-date settings lines)
         (cons
            (string->list
               (date-str (time)
                  (get settings 'utc-offset 0)))
            lines))

      (define (lex-less? a b)
         (cond
            ((null? a) #true)
            ((null? b) #false)
            ((< (car a) (car b)) #true)
            ((= (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #false)))

      (define (lex-sort settings lines)
         (sort lex-less? lines))

      (define (reverse-line-order settings lines)
         (reverse lines))

      (define (reverse-lines settings lines)
         (map reverse lines))

      (define extra-functions
         (list
            (cons "sort"    lex-sort)
            (cons "reverse" reverse-line-order)
            (cons "rev"     reverse-lines)
            (cons "date"    current-date)
            (cons "fmt"     format-merged)
            (cons "crash"   (λ args (car 42)))))

      (define (find-extra name)
         (let ((res (assoc name extra-functions)))
            (if (pair? res)
               (cdr res)
               #false)))))



