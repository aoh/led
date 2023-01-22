
(define-library (led syntax)

   (import
      (owl toplevel)
      (only (owl terminal) font-normal font-reverse font-dim)
      )

   (export
      syntax-highlight)

   (begin

      (define (push-keyword tree lst)
         (cond
            ((null? lst)
               (put tree 'end? #t))
            (else
               (let ((sub (get tree (car lst) empty)))
                  (put tree (car lst)
                     (push-keyword sub (cdr lst)))))))

      (define keywords
         (fold
            (lambda (k s) (push-keyword k (string->list s)))
            empty
            '("define" "if" "let" "let*" "lets" "cond" "else" "lambda"
              "begin" "define-library" "import" "export")))


      ;; → head tail
      (define (grab tree lst)
         (let loop ((tree tree) (lst lst) (rout '()))
            (cond
               ((null? lst)
                  (if (get tree 'end? #f)
                     (values (reverse rout) lst)
                     (values #f lst)))
               ((eq? tree empty)
                  (values #f lst))
               (else
                  (if (get tree 'end? #f)
                     (lets ((hd tl (loop (get tree (car lst) empty) (cdr lst) (cons (car lst) rout))))
                        (if hd
                           (values hd tl)
                           (values (reverse rout) lst)))
                     (loop (get tree (car lst) empty) (cdr lst) (cons (car lst) rout)))))))


      (define define-chars (string->list "define"))

      (define (paren? x)
         (or (eq? x #\() (eq? x #\))))

      (define esc 27)

      (define (dim-string lst cont)
         (cond
            ((null? lst)
               (cont (font-normal lst)))
            ((eq? (car lst) #\")
               (cons (car lst)
                  (font-normal
                     (cont (cdr lst)))))
            (else
               (cons (car lst)
                  (dim-string (cdr lst) cont)))))

      (define (highlight-word lst cont)
         (lets ((hd tl (grab keywords lst)))
            (cond
               ((not hd)
                  (cont lst))
               ((or (null? tl) (eq? (car tl) #\space))
                  (font-dim
                     (append
                        hd
                        (font-normal (cont tl)))))
               (else
                  (cont lst)))))

      (define (syntax-highlight lst)
         (let loop ((lst lst))
            (if (null? lst)
               lst
               (let ((x (car lst)))
                  (cond
                     ((eq? x esc) ;; already coloured. don't touch this.
                        lst)
                     ((paren? x)
                        (font-dim
                           (cons x
                              (font-normal
                                 (loop (highlight-word (cdr lst) syntax-highlight))))))
                     ((eq? x #\;)
                        (append
                           (font-dim lst)
                           (font-normal '())))
                     ((eq? x #\")
                        (font-dim
                           (cons x
                              (dim-string (cdr lst) syntax-highlight))))
                     ((eq? x #\/)
                        (if (and (pair? (cdr lst)) (eq? x (cadr lst)))
                           (append
                              (font-dim lst)
                              (font-normal '()))
                           (cons x (loop (cdr lst)))))
                     (else
                        (cons x (loop (cdr lst)))))))))))
