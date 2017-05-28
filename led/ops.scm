;;;
;;; Primitive edit actions (buffer → buffer')
;;;

(define-library (led ops)
   
   (export 
       op-delete-lines   ;; buff from to target → buff'
       
       buffer-seek seek-line meta-dimensions ;; temp exports
       )
    
   (import
      (owl base)
      (led buffer)
      (led log)
      (led node))
  
   (begin 
      
      (define (meta-dimensions meta)
        (let ((glob (get meta 'global #empty)))
           (values 
              (get glob 'width 20)
              (get glob 'height 10))))    
  
      ;; lines → u line d y
      (define (seek-line lines end)
         (let loop ((lines lines) (pos end) (u null))
            (cond
               ((null? lines)
                  (values u null lines (- end pos)))
               ((null? (cdr lines))
                  (values u (car lines) (cdr lines) (- end pos)))
               ((eq? pos 0)
                  (values u (car lines) (cdr lines) end))
               (else
                  (loop (cdr lines) (- pos 1) (cons (car lines) u))))))
     
      ;; row+1 = y + dy, dy = row + 1 - y
      (define (buffer-seek buff x y screen-y)
         (log "buffer seek" x "," y ", y row at " screen-y)
         (lets ((u d l r old-x old-y off meta buff)
                (w h (meta-dimensions meta))
                (lines (append (reverse u) (list (append (reverse l) r)) d))
                (u line d y (seek-line lines y))
                (step (>> w 1))
                (yp (or screen-y (if (< y h) (+ y 1) (>> h 1)))) ;; real or middle of screen
                (dy (- (+ y 1) yp))
                (buff (buffer u d null line 1 yp off meta)))
               ;; seek right
               (let loop ((xp 1) (pos x) (l null) (r line) (dx 0))
                  (cond
                     ((>= xp w)
                        (loop (- xp step) pos l r (+ dx step)))
                     ((eq? pos 0)
                        (buffer u d l r xp yp (cons dx dy) meta))
                     ((null? r)
                        (loop xp 0 l r dx))
                     (else
                        (loop (+ xp (node-width (car r))) (- pos 1) (cons (car r) l) (cdr r) dx))))))
      
      (define (delete-lines-below buff n target)
         (lets ((u d l r x y off meta buff)
                (d (cons (append (reverse l) r) d))
                (cutd d (split d n))
                (r d (uncons d null)) ;; fixme: temp
                (buff (buffer u d l r x y off meta)))
            (put-copy-buffer buff target
               (tuple 'lines cutd))))
             
      (define (op-delete-lines buff from to target)
         (lets ((buff (buffer-seek buff 0 (- from 1) #false)))
            (delete-lines-below buff (+ 1 (- to from)) target)))
))

