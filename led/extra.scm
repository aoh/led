;;;
;;; Extra processing functions
;;;

(define-library (led extra)

   ;; there are intentionally no led-specific imports
   
   (import
      (owl base)
      (owl date))
  
   (export 
      extra-functions
      find-extra
      )
   
   (begin

      (define (current-date settings lines)
         (list 
            (string->list
               (date-str (time)
                  (get settings 'utc-offset 0)))))
               
      (define (lex-less? a b)
         (cond
            ((null? a) #true)
            ((null? b) #false)
            ((< (car a) (car b)) #true)
            ((= (car a) (car b))
               (lex-less? (cdr a) (cdr b)))
            (else #false)))
     
      (define (lex-sort settings lines)
         (sort lex-less? lines))
      
      (define (reverse-line-order settings lines)
         (reverse lines))
      
      (define (reverse-lines settings lines)
         (map reverse lines))
      
      (define extra-functions
         (list
            (cons "sort"    lex-sort)
            (cons "reverse" reverse-line-order)
            (cons "rev"     reverse-lines)
            (cons "date"    current-date)
            ))
         
      (define (find-extra name)
         (let ((res (assoc name extra-functions)))
            (if (pair? res)
               (cdr res)
               #false)))))
         
       
                        
