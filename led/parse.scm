(define-library (led parse)

   (export
      get-command)

   (import
      (owl base)
      (owl parse))
   
   (begin

      ;; note: could expose notify to get parser state based feedback

      (define (get-key-if pred)
         (let-parses
            ((x get-byte) ;; in this case a terminal event
             (verify (and (tuple? x) (eq? (ref x 1) 'key) (pred (ref x 2))) #false))
            (ref x 2)))

      (define (get-key k)
         (get-key-if 
            (lambda (x) (eq? x k))))
      
      (define (lc-alpha? cp)
         (cond
            ((< cp #\a) #false)
            ((> cp #\z) #false)
            (else #true)))
            
      (define get-buffer-name
         (let-parses
            ((skip (get-key #\"))
             (name (get-key-if lc-alpha?)))
            name))

      (define get-leading-digit
         (get-key-if
            (lambda (x) (<= #\1 x #\9))))
         
      (define get-digit
         (get-key-if
            (lambda (x) (<= #\0 x #\9))))

      (define get-integer
         (let-parses
            ((a get-leading-digit)
             (as (get-greedy* get-digit)))
            (fold (lambda (x a) (+ (* x 10) (- a #\0))) 0 (cons a as))))

      (define get-movement
         (let-parses
            ((a (get-key #\w)))
            'word))

      (define (optional parser default)
         (get-either parser (get-epsilon default)))
      
      (define get-delete
         (let-parses
            ((buff (optional get-buffer-name 'yank))
             (rep  (optional get-integer 1))
             (cmd  (get-key #\d))
             (move get-movement))
            (tuple 'delete buff rep move)))
     
      (define get-command 
         (get-any-of
            get-delete))

      (define (parse-command ll)
         (print "parsing " ll)
         (get-command ll
            (lambda (data fail val pos)
               (values val data))
            (lambda (pos reason)
               (values #false ll))
            0))
         
      (define (try str)
         (print "trying " str)
         (lets ((res ll (parse-command (map (lambda (x) (tuple 'key x)) (string->list str)))))
            (print "'" str "' -> " res " + " ll)))

      (try "\"x42dw***")))
