
;; UI protocol
; input-terminal + _ => depends
; _ + #(open X env commands) => attempt to open a new buffer, or switch to it
; _ + #(add-opener X) => add a new function to perform an open action
; _ + #(buffer-closed)    => drop sending buffer from list
; _ + #(terminal-size w h) => notify sub-buffers
; current + _        => send to screen
; _ + #(yank _)      => notify all buffers that something was copied to yank buffer

(define-library (led ui)

   (import
      (owl toplevel)
      (led screen)
      (led log))

   (export
      start-ui
      ui-put-yank
      ui-get-yank)

   (begin

      ;; protocol helpers

      (define (ui-put-yank text)
         (mail 'ui (tuple 'yank text)))

      (define (ui-get-yank)
         (interact 'ui
            (tuple 'get-yank)))

      (define (refresh window)
         (clear-screen)
         (mail window (tuple 'refresh)))

      ;; manage threads and their views
      ;; decide which ones get to draw on screen

      (define (close-buffer l r)
         (if (null? (cdr l))
            (if (null? r)
               (values #f #f)
               (values (list (car r)) (cdr r)))
            (values (cdr l) r)))

      (define (find-buffer l r p)
         (cond
            ((null? l) (values #f #f))
            ((equal? (car l) p) (values l r))
            (else (find-buffer (cdr l) (cons (car l) r) p))))

      (define (send-commands buffer-id cmds)
         (log "Sending opening commands " cmds " to " buffer-id)
         (fold
            (lambda (id command)
               (mail id (tuple 'eval command)))
            buffer-id cmds))

      ;; buffers are corresponding thread ids
      ;; l[eft], buffers, car is the active one
      ;; r[ight], buffers
      ;; i[nformation], global data
      (define (ui l r i)
         (lets ((msg (wait-mail))
                (from msg msg))
            ; (log "ui: " msg " from " from)
            (cond
               ((eq? from 'input-terminal)
                  (tuple-case msg
                     ((ctrl x)
                        (cond
                           ((eq? x 'n)
                              (if (null? r)
                                 (ui l r i)
                                 (begin
                                    (refresh (car r))
                                    (ui (cons (car r) l) (cdr r) i))))
                           ((eq? x 'p)
                              (if (null? (cdr l))
                                 (ui l r i)
                                 (begin
                                    (refresh (cadr l))
                                    (ui (cdr l) (cons (car l) r) i))))
                           ((eq? x 'h)
                              (let ((r (append (reverse l) r)))
                                 (refresh (car r))
                                 (ui (list (car r)) (cdr r) i)))
                           ((eq? x 'q) ;; close current buffer (from outside), may leave zombies for now
                              (halt 1)
                              (lets ((l r (close-buffer l r)))
                                 (if l
                                    (begin
                                       (refresh (car l))
                                       (ui l r i))
                                    0)))
                           (else
                              (mail (car l) msg)
                              (ui l r i))))
                     (else
                        (mail (car l) msg)
                        (ui l r i))))
               ((eq? (ref msg 1) 'open) ;; #(open <source> <env> <commands>)
                  (log "OPEN " msg)
                  (lets ((lp rp (find-buffer (append (reverse r) l) null (ref msg 2))))
                     (if lp
                        (begin
                           ;; buffer already open: focus and run commands
                           (refresh (car lp))
                           (send-commands (car lp) (ref msg 4))
                           (ui lp rp  i))
                        (lets ((openers (get i 'openers null))
                               (id (fold (lambda (out fn) (or out (fn (ref msg 2) (ref msg 3)))) #f openers)))
                           (if id
                              (begin
                                 ;; buffer opened, send commands
                                 (mail id (tuple 'terminal-size (get i 'width 80) (get i 'height 30)))
                                 (send-commands id (ref msg 4))
                                 (ui (cons id l) r i))
                              (ui l r i))))))
               ((eq? (ref msg 1) 'whoami)
                  (mail from from)
                  (ui l r i))
               ;; yanking
               ((eq? (ref msg 1) 'yank)
                   ;; replace replace yanks with something like (store/load [key] [value])
                  (ui l r
                     (put i 'yank (ref msg 2))))
               ((eq? (ref msg 1) 'get-yank)
                  ;; get-yank (sync) -> yanked data | #f
                  (mail from (get i 'yank #f))
                  (ui l r i))

               ((eq? (ref msg 1) 'add-opener)
                  (log "installing new opener")
                  (ui l r
                     (put i 'openers (cons (ref msg 2) (get i 'openers null)))))
               ((eq? (ref msg 1) 'buffer-closed)
                  (lets ((l (keep (lambda (x) (not (eq? x from))) l))
                         (r (keep (lambda (x) (not (eq? x from))) r)))
                     (if (null? l)
                        (if (null? r)
                           (begin
                              ;; exiting program. leave data on screen.
                              (mail 'screen (tuple 'set-cursor 1 (get i 'height 1)))
                              (mail 'screen (tuple 'clear-line-right))
                              (interact 'screen (tuple 'ping))

                              (halt 0))
                           (begin
                              (refresh (car r))
                              (ui (list (car r)) (cdr r) i)))
                        (begin
                           (refresh (car l))
                           (ui l r i)))))
               ((eq? (ref msg 1) 'terminal-size)
                  (lets ((_ w h msg))
                     (map
                        (λ (id)
                           (log "ui: forwarding terminal-size to " id)
                           (mail id msg))
                        (append l r))
                     (ui l r
                        (pipe i
                           (put 'width w)
                           (put 'height h)))))
               ((eq? from (car l))
                  ;; forward print message from current window
                  (mail 'screen msg)
                  ;(print " - forwarding to screen")
                  (ui l r i))
               ((eq? 'error (ref msg 1))
                  (log "UI received ERROR: " msg " from " from)
                  (ui l r i))
               (else
                  ;(print-to 3 3 "waty" from)
                  ;(print-to 3 4 "watz " msg)
                  (ui l r i)))))

      (define (start-ui)
         (lets ((name 'ui))
            (thread name
               (ui null null empty))
            (link name)
            ;; ui tells the size to client threads
            name))

))

