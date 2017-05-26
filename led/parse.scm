;;;
;;; Ex mode command parser
;;;

(define-library (led parse)

   (export led-parse)   ;; str → #false | command-tuple

   (import
      (owl base)
      (owl parse))
   
   (begin

      (define (get-key-if pred)
         (let-parses
            ((x get-byte) ;; in this case a terminal event
             (verify (and (tuple? x) (eq? (ref x 1) 'key) (pred (ref x 2))) #false))
            (ref x 2)))

      (define get-digit
         (let-parses
            ((x (get-byte-if (λ (x) (<= #\0 x #\9)))))
            (- x #\0)))

      (define get-integer
         (let-parses
            ((as (get-kleene+ get-digit)))
            (fold (λ (x a) (+ (* x 10) a)) 0 as)))

      (define get-position
         (get-either
            get-integer
            (let-parses
               ((skip (get-imm #\.)))
               'dot)))
      
      (define get-command 
         get-position)

      (define (any->ll x)
         (cond
            ((string? x) (str-iter x))
            ((vector? x) (vec-iter x))
            (else x)))
     
      (define (empty-ll? x)
         (cond
            ((null? x) #true)
            ((pair? x) #false)
            (else (empty-ll? (x)))))
         
      ;; iterable → #false | parse-result
       
      (define (led-parse thing)
         (try-parse get-command thing #f #f #f))

      (print (led-parse (any->ll ".")))
))
