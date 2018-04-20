(define (elementary-ws effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))
   
    (define (conn) (hashtable-ref state 'ws '()))
    (define json (js-eval "JSON"))
    (define object (js-eval "Object"))
    
    (define (json-encode obj)
      (js-invoke json "stringify" obj))
    
    (define (json-parse str)
      (js-invoke json "parse" str))

    (define (js-keys obj)
      (js-array->list (js-invoke object "keys" obj)))
    
    (define (array? obj) (not (js-undefined? (js-ref obj "push"))))

    (define (json-decode-value v)
      (case (or (string? v) (number? v))
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
      
    (define (js-key k)
      (case (symbol? k)
        ('#t (symbol->string k))
        ('#f k)))

    (define (js-val v)
      (case (string? v)
        ('#t v)
        ('#f
         (case (symbol? v)
           ('#t (symbol->string v))
           ('#f
            (case (list? v)
              ('#t (list->js v (js-obj)))
              ('#f (console-error "cannot encode " v))))))))


    (define (list->js data r)
      (case (null? data)
        ('#t r)
        ('#f
         (let* ((pair (car data))
                (k (car pair))
                (v (car (cdr pair))))
           (js-set! r (js-key k) (js-val v))
           (list->js (cdr data) r)))))

    (define (ws-connect url)
      (js-new "WebSocket" url))
       
    (define (ws-send data)
      (let* ((js (list->js data (js-obj)))
             (str (json-encode js)))
        (js-invoke (conn) "send" str)))

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
      
    (hashtable-set! state 'ws (connect))

    (list 'ok recv)))
