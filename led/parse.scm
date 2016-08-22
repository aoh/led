(define-library (led parse)

   (export
      get-command)

   (import
      (owl base)
      (led terminal)
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

      (define (key-value key value)
         (let-parses
            ((x (get-key key)))
            value))
      
      (define get-movement
         (get-any-of
            (key-value #\w 'word)
            (key-value #\b 'word-back)
            (key-value #\c 'char)
            (key-value #\h 'left)
            ;(key-value #\j 'down)
            ;(key-value #\k 'up)
            ;(key-value #\l 'right)
            ;(key-value #\( 'sentence-back)
            ;(key-value #\) 'sentence)
            ;(key-value #\} 'paragraph)
            ;(key-value #\{ 'paragraph-back)
            ;(key-value #\0 'line-first)
            ;(key-value #\^ 'line-first-character) ;; non-whitespace
            ;(key-value #\+ 'next-line-first-character)
            ;(key-value #\+ 'previous-line-first-character)
            ;(key-value #\$ 'line-end)
            ;(key-value #\H 'screen-first)
            ;(key-value #\M 'screen-middle)
            ;(key-value #\L 'screen-last)
            ;(let-parses ((n get-integer) (skip (get-key #\|))) (tuple 'char-at n))
            ;(let-parses ((n get-integer) (skip (get-key #\H))) (tuple 'screen-first-plus n))
            ;(let-parses ((n get-integer) (skip (get-key #\L))) (tuple 'screen-last-minus n))
            ))
      
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
         ;(get-any-of get-delete)
         get-movement
         )

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

      (define (try-terminal)
         (set-terminal-rawness #true)
         (let loop ((ll (terminal-input)) (row 1))
            (lets ((res ll (parse-command ll)))
               (write-bytes stdout
                  (tio
                     (set-cursor 1 row)
                     (clear-line)
                     (output res)
                     (set-cursor 1 (+ row 1))))
               (if res
                  (loop ll (+ 1 (modulo row 10)))
                  (set-terminal-rawness #false)))))
                  
      ;(try "\"x42dw***")

      (try-terminal)))
