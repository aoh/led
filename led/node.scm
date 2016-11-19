(define-library (led node)
   (export
      key-node
      rp-node
      lp-node
      left-paren?
      right-paren?
      space-char?
      
      node-screen-width
      node-screen-representation
      node-width
      encode-node
          
      take-printable
      drop-printable
      printable-length
      render-node
            
      tab-node
      hex-node
      whitespace?
      drop-leading-whitespace)
   
   (import
      (owl base)
      (led log)
      (only (owl unicode) encode-point)
      (led terminal))
  
   (begin    
      
      (define rp-node
         (tuple 'replace (list 41) 1 (tio (font-dim) (raw (list 41)) (font-normal))))
         
      (define lp-node
         (tuple 'replace (list 40) 1 (tio (font-dim) (raw (list 40)) (font-normal))))
         
      (define (left-paren? x)
         (or (eq? x 40)
             (equal? x lp-node)))
      
      (define (right-paren? x)
         (or (eq? x 41)
             (equal? x rp-node)))
      
      (define (space-char? x)
         (or (eq? x #\space) (eq? x 9)))
     
      ;; representation of a real tab 
      (define tab-node 
        (tuple 'replace 
         (list #\tab) 
         3 
         (tio
            (font-dim)
            ;(raw (list #\space #\▹ #\space))
         (raw (list #\_ #\_ #\_))
         (font-normal))))
      
      (define (num->hex n)
         (lets ((n (number->string n 16))
                (l (string-length n)))
            (cond
               ((eq? l 1)
                  (ilist #\0 #\x #\0 (string->list n)))
               ((eq? l 2)
                  (ilist #\0 #\x (string->list n)))
               (else
                  (error "num->hex: " n)))))
      
      (define (hex-node n)
         (let ((node
            (tuple 'replace 
               (list n) 
               4 
               (tio
                  (font-dim)
                  (raw (num->hex n))
                  (font-normal)))))
            (log "hex node of " n " is " node)
            node))
      
      (define (whitespace? node)
         (cond
            ((eq? node #\space) #true)
            ((eq? node 13) #true)
            ((eq? node tab-node) #true)
            (else #false)))
      
      (define (drop-leading-whitespace lst)
         (cond
            ((null? lst) lst)
            ((whitespace? (car lst))
               (drop-leading-whitespace (cdr lst)))
            (else lst)))
      
      (define (key-node k meta)
         (cond
            ((eq? k #\tab)
               (get meta 'tab tab-node))
            ((eq? k 40) ;; lp
               ;k
               lp-node
               )
            ((eq? k 41) ;; rp
               ;k
               rp-node
               )
            ((< 31 k 127) ;; ascii range char
               k)
            ((eq? k 127)
               (hex-node k))
            ((< k 32)
               (hex-node k))
            (else
               k)))
            
      (define (node-screen-width x) (ref x 3))
      
      (define (node-screen-representation x) (ref x 4))
      
      (define (node-width x)
         (cond
            ((eq? (type x) type-fix+) 1)
            ((tuple? x) (node-screen-width x))
            (else (error "node-width: " x))))
      
      (define (encode-node k tl)
         (cond 
            ((eq? (type k) type-fix+)
               (encode-point k tl))
            (else
               (foldr encode-node tl (node-screen-representation k)))))

      (define (take-printable line n)
        (cond
          ((eq? n 0) null)
          ((pair? line)
            (lets ((x line line))
              (cond
                ((eq? (type x) type-fix+)
                  ;; a printable unicode code point
                  (if (eq? 0 (fxband x #x80))
                    ;; a printable ascii range thingie (usual suspect)
                    (cons x (take-printable line (- n 1)))
                    (encode-point x
                      (take-printable line (- n 1)))))
                ((tuple? x)
                  ;; #(type actual-codepoints width-on-screen screen-codepoints)
                  (lets ((type cps width output x))
                    (cond
                      ((eq? type 'replace)
                          (append output
                             (take-printable line (- n width))))
                      (else
                        (error "take-printable: what is " x)))))
                (else
                  (error "take-printable: what is " x)))))
          (else
            null)))
      
      (define (render-node node tl)
         (cond
            ((eq? (type node) type-fix+)
               (encode-point node tl))
            ((and (tuple? node) (eq? (ref node 1) 'replace))
               (foldr render-node tl (ref node 2)))
            (else
               (error "render-node: what is " node))))
      
      (define (drop-printable line n)
        (cond
          ((eq? n 0) line)
          ((pair? line)
            (lets ((x line line))
              (cond
                ((eq? (type x) type-fix+)
                  (drop-printable line (- n 1)))
                ((tuple? x)
                  ;; #(type actual-codepoints width-on-screen screen-codepoints)
                  (lets ((type cps width output x))
                     ;(drop-printable (append output line) n)
                     (drop-printable line (- n width))))
                (else
                  (error "drop-printable: what is " x)))))
          (else
            null)))
      
      (define (printable-length line)
         (fold (λ (n x) (+ n (node-width x))) 0 line))))

