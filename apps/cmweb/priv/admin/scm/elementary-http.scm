(define (elementary-http effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))
    
    (define $ (js-eval "$"))
    (define window (js-eval "window"))
    (define location (js-ref window "location"))
    (define object (js-eval "Object"))
    (define json (js-eval "JSON")) 
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

    (define http-url
      (let ((url (get 'url effect-settings)))
        (case url
          ('undef
           (let ((path (get 'path effect-settings))
                 (proto (js-ref location "protocol")))
             (string-append proto "//" (js-ref location "host")  path)))
          (else url))))
    
    (define (http-method m)
      (string-upcase (symbol->string m)))
    

    (define (http-req-on req ev fn)
      (js-set! req ev (js-lambda fn)))
      
    (define (http-headers headers)
      (let ((obj (js-obj)))
        (map (lambda (h)
               (js-set! obj (symbol->string (car h)) (car (cdr h)))) headers)
        obj))
    

    (define (decoded-txt content-type str)
      (case (eq? "application/json" content-type)
        ('#f str)
        ('#t 
         (let* ((parsed (json-parse str))
               (decoded (json-decode (js-keys parsed) parsed '())))
           decoded))))

    (define (http-resp resp)
      (let* ((status (js-ref resp "status"))
             (txt (js-ref resp "responseText"))
             (content-type (js-invoke resp "getResponseHeader" "content-type"))
             (content-length (js-invoke resp "getResponseHeader" "content-length"))
             (decoded (decoded-txt content-type txt)))
        (send (list (list 'effect effect-name)
                    (list 'event 'data)
                    (list 'data decoded)))))

    (define (http-req m h body)
      (let ((req (js-obj
                   "type" (http-method m)
                   "url" http-url
                   "data" body
                   "headers" (http-headers h)
                   "processData" '#f
                   "contentType" '#f 
                   )))
        (http-req-on req "complete" (lambda (resp)
                                      (http-resp (car resp))))
        req))

    (define (http-send req) 
      (let* ((m (get 'method req))
             (h (get 'headers req))
             (b (get 'body req))
             (req (http-req m h b)))
        (js-invoke $ "ajax" req)))

    (define (recv encs enc m) 
      (let ((encoded (encode enc m)))
        (case (car encoded)
          ('ok 
           (http-send (car (cdr encoded))))
          (else (console-error "encode error" (list enc m))))))
    
    (list 'ok recv)))
