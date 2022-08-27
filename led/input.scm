;;;
;;; Terminal Input
;;;
;
; Terminal input is handled by 'input-terminal thread, which only sends terminal
; input events to a specific target thread, which is usually the UI thread. This
; way blocking happens automatically within the input thread, and the rest can deal
; with asynchronous message passing as usual.
;

(define-library (led input)

   (import
      (owl toplevel)
      (led log)
      (owl terminal))

   (export
      start-input-terminal)

   (begin

      (define (input-terminal input target)
         (lfold
            (λ (_ thing)
               ;(log "input terminal: sending " thing " to " target)
               (mail target thing))
            'unused
            input))

      (define (start-input-terminal target ll)
         (let ((name 'input-terminal))
            (thread name
               (input-terminal ll target))
            (link name)
            name))))
