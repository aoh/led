;;;
;;; Ex mode command parser
;;;

(define-library (led parse)

   (export led-parse)   ;; str → #false | command-tuple

   (import
      (owl base)
      (owl parse)
      (owl proof))
   
   (begin

      (define (get-imm-as imm value)
         (let-parses
            ((skip (get-imm imm)))
            value))
      
      (define (maybe-get-imm-as imm was wasnt)
         (get-either
            (get-imm-as imm was)
            (get-epsilon wasnt)))
            
      (define (get-optionally parser default)
         (get-either parser 
            (get-epsilon default)))
     
      ;; ------------------------

      (define allow-whitespace
         (get-greedy*
            (get-byte-if
               (λ (byte)
                  (or (eq? byte #\space)
                      (eq? byte #\tab))))))
                  
      (define get-digit
         (let-parses
            ((x (get-byte-if (λ (x) (<= #\0 x #\9)))))
            (- x #\0)))

      (define get-integer
         (let-parses
            ((as (get-greedy+ get-digit)))
            (fold (λ (x a) (+ (* x 10) a)) 0 as)))
             
      (define get-dot
         (get-imm-as #\. 'dot))
      
      (define get-end-position
         (get-imm-as #\$ 'end))
      
      (define get-position
         (get-any-of
            get-integer
            get-dot
            get-end-position))

      (define interval-everything
         (list 'interval 1 'end))
            
      (define get-interval-everything
         (get-imm-as #\% interval-everything)) 

      (define get-dotted-interval
         (let-parses
            ((start get-position)
             (skip (get-imm #\,))
             (end get-position))
            (list 'interval start end)))
     
      (define get-interval
         (get-any-of
            get-dotted-interval
            get-interval-everything))
     
      ;; write command =  #(write[!] range path)

      (define get-path
         (let-parses
            ((chars
               (get-greedy+
                  (get-rune-if ;; allow-8
                     (λ (x) (not (eq? x #\space)))))))
            (list->string chars)))
      
      (define get-write
         (let-parses
            ((interval 
               (get-optionally get-interval interval-everything))
             (skip (get-imm #\w))
             (operation (maybe-get-imm-as #\! 'write! 'write))
             (skip allow-whitespace)
             (path get-path))
            (list operation interval path)))

      ;; --------------------------
                  
      (define get-command 
         (let-parses
            ((skip allow-whitespace)
             (command 
                get-write)
             (skip allow-whitespace))
            command))
          
      ;; --------------------------

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
         (try-parse get-command (any->ll thing) #f #f #f))

      (example
         (led-parse "1,2w foo.txt") = '(write (interval 1 2) "foo.txt")
         (led-parse "%w! bar.txt") = '(write! (interval 1 end) "bar.txt")
      )
))
