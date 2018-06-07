(define (elementary-ws effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))
   
    (define (conn) (hashtable-ref state 'ws '()))
    (define object (js-eval "Object"))
    (define window (js-eval "window"))
    (define location (js-ref window "location"))
    
    (define (json-encode obj)
      (js-invoke json "stringify" obj))
    
    (define (json-parse str)
      (js-invoke json "parse" str))

    (define (js-keys obj)
      (js-array->list (js-invoke object "keys" obj)))
    
    (define (array? obj) (not (js-undefined? (js-ref obj "push"))))

    (define (json-decode-value v)
      (case (or (string? v) (number? v) (boolean? v))
        ('#t v)
        (else 
          (case (array? v)
            ('#t (map json-decode-value (js-array->list v)))
            (else (json-decode (js-keys v) v '()))))))
              
    (define (json-decode keys obj out)
      (case (length keys)
        ('0 out)
        (else 
          (let* ((k (car keys))
                (v (js-ref obj k))
                (decoded-v (json-decode-value v)))
            (json-decode (cdr keys) obj (set (string->symbol k) decoded-v out)))))) 
      

    (define (ws-connect url)
      (js-new "WebSocket" url))
    
    (define (ws-send-str str)
      (js-invoke (conn) "send" str))

    (define (ws-send data)
      (let* ((js (list->js data (js-obj)))
             (str (json-encode js)))
        (ws-send-str str)))

    (define (ws-on socket event fn)
      (js-set! socket event (js-lambda fn)))

    (define (ws-url)
      (let ((url (get 'url effect-settings)))
        (case url
          ('undef
           (let ((path (get 'path effect-settings))
                 (proto (ws-proto)))
             (string-append proto "//" (js-ref location "host")  path)))
          (else url))))
      
    (define (ws-proto)
      (let ((proto (js-ref location "protocol")))
        (case (eq? "http:" proto)
          ('#t "ws:")
          ('#f "wss:"))))

    (define (ws-persistent?)
      (eq? 'true (get 'persistent effect-settings)))
    
    (define (connect)
      (let ((url (ws-url)))
        (case url
          ('undef (console-error "no websocket url in settings" effect-settings))
          (else 
            (let ((ws (ws-connect url)))
              (ws-on ws "onopen" (lambda (socket) 
                                   (start-keep-alive?)
                                   (send (list (list 'effect effect-name) 
                                                                (list 'event 'connected)))))
              (ws-on ws "onclose" (lambda (socket) 
                                        (start-new-ws?)
                                        (send (list (list 'effect effect-name) 
                                                  (list 'event 'disconnected)))))
              (ws-on ws "onerror" (lambda (socket) 
                                      (send (list (list 'effect effect-name)
                                                  (list 'event 'error)))))
              (ws-on ws "onmessage" (lambda (event)
                                      (let* ((str (js-ref (car event) 'data))
                                             (parsed (json-parse str))
                                             (decoded (json-decode (js-keys parsed) parsed '())))
                                      (send (list (list 'effect effect-name)
                                                  (list 'event 'data)
                                                  (list 'data decoded))))))

              ws)))))

    (define (recv encs enc m) 
      (let ((encoded (encode enc m)))
        (case (car encoded)
          ('ok 
           (ws-send (car (cdr encoded))))
          (else (console-error "decode error" (list enc m))))))
    
    (define (start-new-ws)
        (hashtable-set! state 'ws (connect)))
    
    (define (start-new-ws?)
      (case (ws-persistent?)
        ('#t 
         (console-log "reconnecting" (ws-url))
         (start-new-ws))))
    
    (define (start-keep-alive?)
      (case (ws-persistent?)
        ('#t (keep-alive))))

    (define (keep-alive)
      (ws-send-str "")
      (hashtable-set! state 'timer (timer keep-alive 20)))

    (define (cancel-keep-alive)
      (let ((timer (hashtable-ref state 'timer 'undef)))
        (case timer
          ('undef 'no-timer)
          (else (clear-timer! timer)))))

    (start-new-ws)

    (list 'ok recv)))
