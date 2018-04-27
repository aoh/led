;;;
;;; Ex mode command parser
;;;

(define-library (led parse)

   (export led-parse)   ;; str → #false | command-tuple

   (import
      (owl base)
      (owl parse)
      (owl proof)
      (owl regex)
      (led log)
      )
   
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
      
      (define (get-imm-optionally value)
         (get-either (get-imm value)
            (get-epsilon #false)))
     
      ;; ------------------------

      (define whitespace-char?
         (λ (byte)
            (or (eq? byte #\space)
                (eq? byte #\tab))))
       
      (define allow-whitespace
         (get-greedy*
            (get-byte-if whitespace-char?)))
                  
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
      
      (define get-sign
         (get-either
            (get-imm-as #\+ '+)
            (get-imm-as #\- '-)))
      
      (define get-delta
         (let-parses
            ((sign get-sign)
             (n get-integer))
            (list sign 'dot n)))
     
      (define (lowercase-char? x)
         (<= #\a x #\z))
      
      (define get-label
         (let-parses
            ((skip (get-imm #\'))
             (label 
                (get-either 
                   (get-byte-if lowercase-char?)
                   (get-imm  #\'))))
            (list 'label label)))
         
      (define get-position
         (any
            get-integer
            get-delta
            get-dot
            get-end-position
            get-label))

      (define interval-everything
         (list 'interval 1 'end))
            
      (define interval-current-line
         (list 'interval 'dot 'dot))
      
      (define get-interval-everything
         (get-imm-as #\% interval-everything)) 

      (define get-dotted-interval
         (let-parses
            ((start get-position)
             (skip (get-imm #\,))
             (end get-position))
            (list 'interval start end)))

      (define get-single-line-position
         (let-parses
            ((pos get-position))
            (list 'interval pos pos)))
           
      (define get-interval
         (any
            get-dotted-interval
            get-interval-everything
            get-single-line-position))
     
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
             (skip allow-whitespace)
             (skip (get-imm #\w)) ; todo: add command abbreviations later
             (operation (maybe-get-imm-as #\! 'write! 'write))
             (skip allow-whitespace)
             (path get-path))
            (list operation interval path)))

      (define get-delete
         (let-parses
            ((interval 
               (get-optionally get-interval interval-current-line))
             (skip allow-whitespace)
             (skip (get-imm #\d))
             (skip allow-whitespace)
             (target (get-either (get-byte-if lowercase-char?) ;; optional buffer name
                                 (get-epsilon 'yank))))
            (list 'delete interval target)))

      (define get-put
         (let-parses
            ((place (get-optionally get-position 'dot))
             (skip allow-whitespace)
             (skip (get-imm #\p))
             (skip (get-imm #\u))
             (skip (get-imm-optionally #\t))
             (skip allow-whitespace)
             (reg (get-either (get-byte-if lowercase-char?)
                              (get-epsilon 'yank))))
            (list 'put place reg)))
         
      (define get-lisp
         (let-parses
            ((place 
               (get-optionally get-interval interval-current-line))
             (skip allow-whitespace)
             (skip (get-imm #\l))
             (skip allow-whitespace)
             (name (get-greedy+ (get-rune-if (λ (x) (not (whitespace-char? x)))))))
            (list 'lisp place (list->string name))))
     
      ;; could use the one from (owl regex) later directly 
      (define get-replace-regex
         (let-parses
            ((skip (get-imm #\s))
             (rest (get-greedy* get-rune))
             (func (eval (string->regex (list->string (cons #\s rest)))))
             (verify func ""))
            func))
         
      (define get-replacement
         (let-parses
            ((skip allow-whitespace)
             (place (get-optionally get-interval interval-current-line))
             (skip allow-whitespace)
             (rep get-replace-regex))
            (list 'lisp-apply place 
               (λ (lines) (map rep lines)))))
               
      ;; --------------------------
                  
      (define get-command 
         (let-parses
            ((skip allow-whitespace)
             (command 
                (any
                  get-write
                  get-delete
                  get-put
                  get-lisp
                  get-replacement))
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
         (log "parsing " thing)
         (let ((res (try-parse get-command (any->ll thing) #f #f #f)))
            (log "parsed " res)
            res))

      (example
         (led-parse "1,2w foo.txt") = '(write  (interval 1 2)   "foo.txt")
         (led-parse "% w! bar.txt") = '(write! (interval 1 end) "bar.txt")
         (led-parse "-1,+2wx")      = '(write (interval (- dot 1) (+ dot 2)) "x")
         (led-parse "'a,'bwx")      = '(write (interval (label #\a) (label #\b)) "x")
         (led-parse ".,$ da")       = '(delete (interval dot end) #\a)
         (led-parse "%d")           = '(delete (interval 1 end) yank)
         (led-parse "put")          = '(put dot yank)
         (led-parse "-3pux")        = '(put (- dot 3) #\x)
         (led-parse "'a,. l sort")  = '(lisp (interval (label #\a) dot) "sort")
      )
))
