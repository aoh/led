(define-library (led system)
   
   (export
      led-dir->list)
   
   (import
      (owl base)
      (owl sys)
      (led log))
   
   (begin

      (define (sort-paths paths)
         (sort string<?
            (remove m/^\./ paths)))
      
      (define (led-dir->list path)
         (let ((contents (dir->list path)))
            (if contents
               (sort-paths contents)
               #false)))))
      
