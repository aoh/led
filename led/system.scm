(define-library (led system)
   
   (export
      directory? ;; temporary
      led-dir->list
      led-dir-recursive->list)
   
   (import
      (owl base)
      (owl sys)
      (led log))
   
   (begin

      (define (sort-paths paths)
         (sort string<?
            (remove m/^\./ paths)))
      
      (define (led-dir-recursive->list path)
         (if (directory? path)
            (let 
               ((sorted-subs 
                  (map
                     (λ (x) (str path "/" x))
                     (sort-paths (dir->list path)))))
               (foldr append null
                  (map led-dir-recursive->list sorted-subs)))
            (list path)))
         
      (define (led-dir->list path)
         (let ((contents (dir->list path)))
            (if contents
               (map 
                  (λ (x) (str path "/" x))
                  (sort-paths contents))
               #false)))))
      
