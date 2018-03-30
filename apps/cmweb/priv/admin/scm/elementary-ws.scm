(define (elementary-ws effect-name effect-settings send)
  (console-log "init effect" effect-name effect-settings)

  (define (recv encs enc m)
    (console-log "received" (list encs enc m)))
  (list 'ok recv))
