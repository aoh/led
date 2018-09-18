;;;
;;; Extra processing functions
;;;

(define-library (led extra)

   ;; there are intentionally no led-specific imports
   
   (import
      (owl base))
  
   (export 
      extra-functions
      find-extra
      )
   
   (begin
      
      (define (lex-less? a b)
         (cond
            ((null? a) #true)
            ((null? b) #false)
            ((< (car a) (car b)) #true)
            ((= (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #false)))
     
      (define (lex-sort lines)
         (sort lex-less? lines))
       
      (define extra-functions
         (list
            (cons "sort" lex-sort)))
         
      (define (find-extra name)
         (let ((res (assoc name extra-functions)))
            (if (pair? res)
               (cdr res)
               #false)))))
         
       
                        
