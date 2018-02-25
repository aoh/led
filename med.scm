(import (owl unicode))

(define (set-terminal-rawness rawp)
   (sys-prim 26 rawp #f #f))

(define (terminate!)
   (set-terminal-rawness #false)
   (halt 0))

(define log-fd 
   (open-output-file "med.log"))

(define (log . stuff)
   (print-to log-fd stuff))
      
(define (terminal-stream target)
   (lfold
      (λ (state n)
         (if (eq? n 3)
            (if (eq? state 0)
               ;; second break, halt
               (terminate!)
               (begin
                  (print-to stderr "break again to exit")
                  0))
            (begin
               (mail target n)
               1)))
      1
      (utf8-decoder
         (port->byte-stream stdin)
         (λ (loop line ll)
            ;; -- bad utf-8 input
            (print-to stderr "Bad UTF-8 in terminal input")
            null))))

(define (poll)
   (lets ((env (wait-mail)))
      (values (ref env 1) (ref env 2))))

(define (num->bytes n tl)
   (cond
      ((eq? n 1) (cons #\1 tl))
      ((eq? (type n) type-fix+)
        (append (string->list (number->string n 10)) tl))
      (else
        (print-to stderr "num->bytes: bad pos " n)
        (cons #\0 tl))))

(define (cursor-pos x y tl)
   (ilist 27 #\[ (num->bytes y (cons #\; (num->bytes x (list #\f))))))
   
(define (set-cursor x y tl)
   (ilist 27 #\[ (num->bytes (max 0 y) (cons #\; (num->bytes (max 0 x) (cons #\f tl))))))
   
(define (clear-line lst)       (ilist 27 #\[ #\2 #\K lst))
(define (clear-line-right lst) (ilist 27 #\[ #\K lst))
(define (clear-line-left lst)  (ilist 27 #\[ #\1 #\K lst))
(define (clear-screen lst) (ilist 27 #\[ #\2 #\J lst))
(define (clear-screen-top lst) (ilist 27 #\[ #\1 #\J lst))
(define (clear-screen-bottom lst) (ilist 27 #\[ #\J lst))
 
;; render-screen! new old → new 
(define (render-screen! new old)
   (write-bytes stdout 
      (clear-screen (set-cursor 5 5 
         (render new'()))))
   new)

;; switch-to! id → id
(define (switch-to! id)
   (mail 'screen
      (tuple 'switch-to id))
   id)

(define (screen active views last)
   (log "screen waiting")
   (lets ((from msg (poll)))
      (log "screen got update from" from)
      (cond
         ((tuple? msg)
            (tuple-case msg
               ;; switch buffer and refresh screen 
               ((switch-to id)
                  (log "switching to buffer" id)
                  (let ((val (get views id '(42 42 42))))
                     (log "val is " val)
                     (screen id 
                        (put views active last) ;; store current view
                        (render-screen! val last))))
               (else
                  (log "screen wat " msg)
                  (screen active views last))))
         ((eq? from active)
            ;; update currently active screen
            (screen active views 
               (render-screen! msg last)))
         (else
            ;; update background active screen
            (log "update @ " from)
            (screen active
               (put views from msg)
               last)))))
                     
(define (update data)
   (mail 'screen data)
   data)

(define (cdr* data)
   (if (pair? data)
      (cdr data)
      data))

(define (scratch-buffer data)
   (lets ((from msg (poll)))
      (log "scratch buffer got" msg "from" from)
      (cond
         ((eq? msg 'redraw)
            (scratch-buffer data))
         ((eq? msg 127)
            (scratch-buffer (update (cdr* data))))
         (else
            (scratch-buffer (update (cons msg data)))))))

(define (fork-buffer! id)
   (fork-linked-server id
      (λ () (scratch-buffer (string->list (symbol->string id))))))
    
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
               ((eq? msg 2)
                  ;; create new buffer
                  (let ((id (gensym (list l this r))))
                     (fork-buffer! id)
                     (buffers l (switch-to! id) (cons this r))))
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
   (log "sender sending to" to "byte" byte)
   (mail to byte)
   (sender to byte interval))

(define (start)
   (print "starting")
   (print "terminal rawness: "
      (set-terminal-rawness #true))

   (fork-buffer! 'scratch)
    
   (fork-linked-server 'screen
      (λ ()
         (screen 'scratch-1 empty null)))

   (fork-linked-server 'echoer-1 ;; send to scratch-1 every 10s
      (λ () (sender 'g1 42 10000)))
   
   (fork-linked-server 'echoer-2 ;; send to scratch-2 every 10s
      (λ () (sender 'g2 97 2000)))
        
   (fork-linked-server 'buffers
      (λ ()
         (buffers null (switch-to! 'scratch) null)))
   
   (fork-linked-server 'terminal
      (λ ()
         (terminal-stream 'buffers)))
   
   (wait-threads))
   
(λ (args)
   (start))






