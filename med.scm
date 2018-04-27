(import 
   (owl unicode)
   (owl date))

(define (set-terminal-rawness rawp)
   (sys-prim 26 rawp #f #f))

(define (terminate!)
   (set-terminal-rawness #false)
   (halt 0))

(define log-fd 
   (open-output-file "med.log"))

(define (log . stuff)
   (print-to log-fd stuff))

(define (decimal-val c)
   (let ((c (- c #\0)))
      (cond
         ((< c 0) #false)
         ((> c 9) #false)
         (else c))))

(define (state-expect-decimal val cont)
   (λ (b)
      (let ((d (decimal-val b)))
         (if d
            (state-expect-decimal (+ (* val 10) d) cont)
            (cont val b)))))
      
(define (state-expect data succ fail)
   (if (null? data)
      succ
      (λ (b)
         (cond
            ((eq? b (car data))
               ;; limited successs
               (state-expect (cdr data) succ fail))
            ((eq? (car data) 'decimal)
               (let ((d (decimal-val b)))
                  (if d 
                     (state-expect-decimal d
                        (λ (res next)
                           (if res
                              ((state-expect (cdr data) (succ res) fail) next)
                              fail)))
                     (fail b))))
            (else 
               (fail b))))))

(define (num->bytes n tl)
   (cond
      ((eq? n 1) (cons #\1 tl))
      ((eq? (type n) type-fix+)
        (append (string->list (number->string n 10)) tl))
      (else
        (print-to stderr "num->bytes: bad pos " n)
        (cons #\0 tl))))

(define (set-cursor x y tl)
   (ilist 27 #\[ (num->bytes (max 0 y) (cons #\; (num->bytes (max 0 x) (cons #\f tl))))))
   
(define terminal-state-machine
      
   (define (state-maybe-halt b)
      (if (eq? b 3)
         (terminate!)
         state-start))
   
   (define (state-start b)
      (cond
         ((eq? b 3) ;; C-c, maybe exit
            (print-to stderr "break again to exit")
            state-maybe-halt)
         ((eq? b 12) ;; C-l, request terminal update
            ;; request cursor position
            (write-bytes stdout (list 27 #\[ #\6 #\n))
            (state-expect 
               (list 27 #\[ 'decimal #\; 'decimal #\R)
               (λ (col)
                  (λ (row)
                     (write-bytes stdout
                        (set-cursor 4095 4095 (list 27 #\[ #\6 #\n)))
                     (state-expect 
                        (list 27 #\[ 'decimal #\; 'decimal #\R)
                        (λ (rows)
                           (λ (cols)
                              ;; send update from here
                              (log "terminal size is " (cons rows cols))
                              (mail 'screen
                                 (tuple 'set-dimension rows cols))
                              ;; restore cursor
                              (write-bytes stdout (set-cursor row col null))
                              state-start))
                        state-start)))
               state-start))
         (else
            (mail 'buffers b)
            state-start)))
   
   state-start)
   
(define (terminal-stream ll)
   (lfold (λ (state n) (state n))
      terminal-state-machine ll))

(define (poll)
   (lets ((env (wait-mail)))
      (values (ref env 1) (ref env 2))))

(define (cursor-pos x y tl)
   (ilist 27 #\[ (num->bytes y (cons #\; (num->bytes x (list #\f))))))
   
(define (clear-line lst)       (ilist 27 #\[ #\2 #\K lst))
(define (clear-line-right lst) (ilist 27 #\[ #\K lst))
(define (clear-line-left lst)  (ilist 27 #\[ #\1 #\K lst))
(define (clear-screen lst) (ilist 27 #\[ #\2 #\J lst))
(define (clear-screen-top lst) (ilist 27 #\[ #\1 #\J lst))
(define (clear-screen-bottom lst) (ilist 27 #\[ #\J lst))



(define (maybe-simple-rows draw-state)
   (tuple-case draw-state
      ((simple x y rows) rows)
      (else null)))


(define (take-upto lst n out)
   (cond
      ((eq? n 0) out)
      ((null? lst) out)
      (else
         (cons (car lst)
            (take-upto (cdr lst) (- n 1) out)))))

(define (car* x)
   (if (pair? x) (car x) x))

(define (cdr* x)
   (if (pair? x) (cdr x) x))

;; screen must end up with rows and cursor at (x, y)
(define (render-simple! x y rows nrows ncols old-rows)
   (lets ((now (string->bytes (date-str (time))))
          (now-row (max 1 nrows))
          (now-col (max 1 (- ncols (length now))))
          (finale
            (set-cursor now-col now-row 
              (append now (set-cursor x y null)))))
      (write-bytes stdout
         (let loop ((rows rows) (old old-rows) (y 1) (out finale))
            (cond
               ((null? rows) 
                  (loop '((#\~)) old y out))
               ((eq? y nrows) out)
               ((eq? rows old) out)
               ((equal? (car rows) (car* old))
                  (loop (cdr rows) (cdr* old) (+ y 1) out))
               (else
                  (loop (cdr rows) (cdr* old) (+ y 1)
                     (set-cursor 1 y 
                        (clear-line-right 
                           (take-upto (car rows) ncols out))))))))))

;; render-screen! new old → new 
(define (render-screen! new old rows cols)
   ;(log "rendering screen of dimensions" (cons cols rows))
   (tuple-case new
      ((simple x y rowdata)
         (render-simple! x y rowdata rows cols (maybe-simple-rows old)))
      (else
         (log "render: not sure how to render" new)))
   new)

;; switch-to! id → id
(define (switch-to! id)
   (mail 'screen
      (tuple 'switch-to id))
   id)



(define (screen rows cols active views last)
   ;(log "screen waiting " (cons rows cols))
   (lets ((from msg (poll)))
      ;(log "screen got update from" from)
      (cond
         ((tuple? msg)
            (tuple-case msg
               ;; switch buffer and refresh screen 
               ((switch-to id)
                  (log "switching to buffer" id)
                  (let ((val (get views id '())))
                     ;(log "val is " val)
                     (screen rows cols id 
                        (put views active last) ;; store current view
                        (render-screen! val last rows cols))))
               ((set-dimension rows cols)
                  (screen rows cols active views last))
               ((simple x y rowdata)
                  (screen rows cols active views
                     (render-screen! msg last rows cols)) )
               ((ping)
                  (screen rows cols active views
                     (render-screen! last last rows cols)))
               (else
                  (log "screen wat " msg)
                  (screen rows cols active views last))))
         ((eq? from active)
            ;; update currently active screen
            (screen rows cols active views 
               (render-screen! msg last rows cols)))
         (else
            ;; update background active screen
            ;(log "update @ " from)
            (screen rows cols active (put views from msg) last)))))
                     
(define (update! x y data)
   (mail 'screen (tuple 'simple x y data))
   data)

(define (scratch-buffer x y rows)
   (lets ((from msg (poll)))
      (log "scratch buffer got" msg "from" from)
      (cond
         ((eq? msg 'redraw)
            (scratch-buffer x y rows))
         ((eq? msg 127)
            (if (= x 1)
               (scratch-buffer x y rows)
               (scratch-buffer (- x 1) y
                  (update! (- x 1) y
                     (cons
                        (reverse (cdr (reverse (car rows))))
                        (cdr rows))))))
         (else
            (scratch-buffer (+ x 1) y 
               (update! (+ x 1) y
                  (cons
                     (append (car rows) (list msg))
                     (cdr rows))))))))

(define (fork-buffer! id)
   (thunk->thread id
      (λ () (scratch-buffer 1 1 (list (list))))))
    
(define (buffers l this r)
   (log "buffers waiting for input")
   (lets ((from msg (poll)))
      (log "buffers got" msg "from" from)
      (cond
         ((eq? from 'terminal)
            (cond
               ((eq? msg 14) ; C-n, next
                  (if (null? r)
                     (buffers l this r)
                     (buffers (cons this l) (switch-to! (car r)) (cdr r))))
               ((eq? msg 16) ; C-p, previous
                  (if (null? l)
                     (buffers l this r)
                     (buffers (cdr l) (switch-to! (car l)) (cons this r))))
               ((eq? msg 2) ; C-b, create buffer
                  (let ((id (gensym (list l this r))))
                     (fork-buffer! id)
                     (buffers (cons this l) (switch-to! id) r)))
               ((eq? msg 23) ; C-w, close buffer
                  (if (null? r)
                     (if (null? l)
                        (terminate!)
                        (buffers (cdr l) (switch-to! (car l)) null))
                     (buffers l (switch-to! (car r)) (cdr r))))
               (else
                  (mail this msg)
                  (buffers l this r))))
         (else
            (log "odd " (list from msg))
            (buffers l this r)))))

(define (wait-threads)
   (let ((env (wait-mail)))
      (print "waited thread: " env)
      (log env)
      (wait-threads)))

(define (sender to byte interval)
   (sleep interval)
   ;(log "sender sending to" to "byte" byte)
   (mail to byte)
   (sender to byte interval))

(define (start)
   (set-terminal-rawness #true)
   (write-bytes stdout
      (set-cursor 1 1 (clear-screen null)))

   (fork-buffer! 'scratch)
    
   (thunk->thread 'screen
      (λ ()
         (screen 10 10 'scratch-1 empty null)))

   (mail 'screen 
      (tuple 'simple 1 1 null))
   
   (thunk->thread 'echoer-1
      (λ () (sender 'g1 42 10000)))
   
   (thunk->thread 'echoer-2
      (λ () (sender 'g2 97 13000)))
        
   (thunk->thread 'buffers
      (λ ()
         (buffers null (switch-to! 'scratch) null)))
  
   (thunk->thread 'pinger
      (λ () (sender 'screen (tuple 'ping) 1000))) 
   
   (thunk->thread 'terminal
      (λ ()
         (cons 12 ;; start with a C-l to update info
            (terminal-stream 
                  (utf8-decoder
                     (port->byte-stream stdin)
                     (λ (loop line ll)
                        ;; -- bad utf-8 input
                        (print-to stderr "Bad UTF-8 in terminal input")
                        null))))))
   
   (wait-threads))
   
(λ (args)
   (start))






