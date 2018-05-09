(define (elementary-events effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))

    (define (send-event data) 
      (send (set 'effect effect-name data)))

    (define (recv encs enc m)
      (let ((encoded (encode enc m)))
        (case (car encoded)
          ('ok
           (timer (lambda ()
                    (send-event (car (cdr encoded)))) 0))
          (else (console-error "encode error" (list enc m))))))
    
    (list 'ok recv)))
