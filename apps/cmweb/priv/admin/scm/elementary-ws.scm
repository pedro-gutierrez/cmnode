(define (elementary-ws effect-name effect-settings send)
  
  (define json (js-eval "JSON"))

  (define (json-encode obj)
    (console-log "stringfy" obj)
    (js-invoke json "stringify" obj))

  (define (ws-connect url)
    (js-new "WebSocket" url))

  (define (ws-on socket event fn)
    (js-set! socket event (js-lambda fn)))

  (define (ws-url) (get 'url effect-settings))

  (define (connect)
    (let ((url (ws-url)))
      (case url
        ('undef (console-error "no websocket url in settings" effect-settings))
        (else 
          (let ((ws (ws-connect url)))
            (ws-on ws "onopen" (lambda (socket) (send (list (list 'effect effect-name) 
                                                            (list 'event 'connected)))))
            (ws-on ws "onclose" (lambda (socket) 
                                  (send (list (list 'effect effect-name) 
                                                            (list 'event 'disconnected)))))
            (ws-on ws "onerror" (lambda (socket) 
                                  (send (list (list 'effect effect-name)
                                             (list 'event 'error))))))))))

  (define (extract-values enc in out)
    (case (length enc)
      ('0 (list 'ok out))
      (else 
        (let* ((spec (car enc))
               (k (car spec))
               (value-spec (car (cdr spec)))
               (value (extract-value value-spec in)))
          (console-log "extracted value" value)
          (case (car value)
            ('ok (extract-values (cdr enc) in (set k (car (cdr value)) out)))
            (else value))))))
  
  (define (recv encs enc m) 
    (let ((encoded (extract-values enc m '())))
      (case (car encoded)
        ('ok (console-log "ws encoded" encoded))
        (else (console-error "decode error" (list enc m))))))

  (connect)

  (list 'ok recv))
