;;;
;;; Abbreviations
;;;

(define-library (led abbreviate)

   (import
      (owl toplevel)
      (led log))

   (export
      add-abbreviation   ;; env abbr-chars abbr-val -> env'
      abbreviate         ;; env lst -> lst'
      )

   (begin

      ;; temp datastrcuture: list of prod of (from-chars to-chars dx)

      (define (add-abbreviation env from-chars to-chars)
         (let ((as (get env 'abbreviations '())))
            (put env 'abbreviations
               (cons
                  (prod
                     (reverse from-chars)
                     (reverse to-chars)
                     (- (length to-chars) (length from-chars)))
                  as))))

      ;; lst pat → lst' | #f
      (define (match-prefix lst cs)
         (cond
            ((null? cs) lst)
            ((null? lst) #f)
            ((eq? (car lst) (car cs))
               (match-prefix (cdr lst) (cdr cs)))
            (else
               #f)))

      ;; env lst → lst' dx
      (define (abbreviate env lst)
         (let ((as (get env 'abbreviations '())))
            ;; temp linear scan version
            (let loop ((as (get env 'abbreviations '())))
               (if (null? as)
                  (values lst 0)
                  (lets
                     ((from to dx <- (car as))
                      (lstp (match-prefix lst from)))
                     (if lstp
                        (values (append to lstp) dx)
                        (loop (cdr as))))))))


))

