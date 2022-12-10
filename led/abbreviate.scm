;;;
;;; Abbreviations
;;;
;
; Abbreviations allow text being written interactively to be replaced
; with other text. Abbreviations are checked after space and newline.
;

(define-library (led abbreviate)

   (import
      (owl toplevel)
      (led log))

   (export
      add-abbreviation   ;; env abbr-chars abbr-val → env'
      abbreviate         ;; env lst → lst'
      maybe-abbreviate   ;; env b char → b' dx
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

      ;; env b char → b' dx
      (define (maybe-abbreviate env b char)
         (if (or (eq? char #\space)
                 (eq? char #\newline))
            (lets
               ((pos l r len line <- b)
                (l dx (abbreviate env l)))
               (values
                  (prod pos l r len line)
                  dx))
            (values b 0)))

))

