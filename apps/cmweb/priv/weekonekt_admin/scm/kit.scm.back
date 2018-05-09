(define doc (js-eval "document"))
(define body (js-ref doc "body"))
(define maquette (js-eval "maquette"))
(define projector (js-invoke maquette "createProjector" ))
(define h* (js-ref maquette "h"))

(define (js-lambda fn)
  (js-closure (lambda args
                  (apply fn (list args)))))

(define (alist-get k model)
  (let ((entry (assoc k model)))
    (case entry
        ('#f 'undef)
        (else (car (cdr entry))))))

(define (alist-set k v model)
    (cons (list k v) model))

(define (flatten l)
  (cond
    [(null? l) '()]
    [(list? l)
     (append (flatten (car l))
             (flatten (cdr l)))]
    [else (list l)]))

(define (h elem attrs children)
  (let ((attrs2 (apply js-obj (flatten attrs)))
        (children2 (list->js-array children)))
    (js-call h* elem attrs2 children2)))

(define (mount-projector fn)
  (js-invoke projector "append" body 
    (js-lambda fn)))

(define json (js-eval "JSON"))

(define (json-encode obj)
    (console-log "stringfy" obj)
    (js-invoke json "stringify" obj))

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
              ('#t (list->json v (js-obj)))
              ('#f (console-error "cannot encode " v))))))))

(define (list->json data r)
    (console-log "converting into json" data)
    (case (null? data)
      ('#t r)
      ('#f
        (let* ((pair (car data))
               (k (car pair))
               (v (car (cdr pair))))
            (js-set! r (js-key k) (js-val v))
            (list->json (cdr data) r)))))

(define (json-decode str)
    (js-invoke json "parse" str))

(define (decode-first prev rules)
  (case (null? rules)
    ('#t '(error nomatch))
    ('#f 
        (let* ((first (car rules))
               (next (decode-match prev (car (cdr first)) (car first))))
          (case (car next)
            ('ok next)
            (else (decode-first prev (cdr rules))))))))

(define (decode-match data rules msg)
  (console-log "matching " msg)
  (case (car data)
    ('ok (decode-match-props (car (cdr data)) rules '() msg))
    (else '(error data))))

(define (decode-match-props data rules result msg)
  (case (null? rules)
    ('#t (list 'ok result msg))
    ('#f 
        (let* ((rule (car rules))
               (key (car rule))
               (value (car (cdr rule)))
               (js-key (js-ref data (symbol->string key))))
          (console-log "decode-match-props - rule" rule)
          (console-log "decode-match-props - key" key)
          (console-log "decode-match-props - value" value)
          (console-log "decode-match-props - js-key" js-key)
          (case (or (js-undefined? js-key) (js-null? js-key))
            ('#t 
                (console-log "decode-match-props missing" key) 
                (list 'error (list 'missing key)))
            ('#f
                (case (symbol? value)
                    ('#t 
                        (console-log "decode-match-props parsing type" js-key)
                        (case value
                          ('text (decode-match-props data
                                    (cdr rules) (alist-set key js-key result) msg))
                          ('string (decode-match-props data
                                    (cdr rules) (alist-set key js-key result) msg))
                          ('number 
                            (let ((num (string->number js-key)))
                              (case (number? num)
                                ('#t (decode-match-props data
                                    (cdr rules) (alist-set key num result) msg))
                                ('#f '(error wrong-value)))))
                          (else '(error wrong-value-type))))
                    ('#f 
                        (case (string? value)
                          ('#t 
                            (case (eq? value js-key)
                              ('#t (decode-match-props data 
                                    (cdr rules) (alist-set key value result) msg))
                              ('#f '(error wrong-value)))) 
                          ('#f 
                            (case (list? value) 
                              ('#t 
                                (console-log "decode-match-props list")
                                (let ((nested (decode-match-props js-key value '() msg)))
                                    (case (car nested)
                                        ('error nested)
                                        ('ok
                                            (console-log "decode-match-props nested" nested)
                                            (decode-match-props data
                                    (cdr rules) (alist-set key (car (cdr nested)) result) msg)))))
                              ('#f 
                                (console-log "decode-match-props unknown rule " value)
                                '(error wrong-value-type)))))))))))))


(define (ws url)
  (js-new "WebSocket" url))

(define (ws-on socket event callback)
  (js-set! socket event 
           (js-closure (lambda args (apply callback (cons socket args))))))
        
;(define (ws-send socket data)
;    (let ((str (encode data)))
;        (js-invoke socket "send" str)))

(define (app init view decode encode update subscriptions)
  (let ((state (make-eq-hashtable)))
    
    (define (model) 
      (hashtable-ref state 'model '()))
    
    (define (subs) 
      (hashtable-ref state 'subscriptions '()))

    (define (add-sub params) 
      (let ((n (car params))
            (s (car (cdr params))))
        (hashtable-set! state 'subscriptions (alist-set n s (subs)))))
    
    (define (with-sub n next)
      (next (car (cdr (assoc n (hashtable-ref state 'subscriptions '()))))))
    
    (define (send-cmd cmd)
        (case (and (list? cmd) (> (length cmd) 1))
          ('#t 
            (let ((s (car cmd)))    
              (with-sub s (lambda (sub) (sub (cdr cmd))))))  
          ('#f (console-log "unknown cmd" cmd))))
    
    (define (schedule-render)
      (js-invoke projector "scheduleRender"))

    (define (update-model props m)
      (console-log "updating model props" props)
      (console-log "updating model model" m)
      (case (null? props)
        ('#t m)
        ('#f 
            (let ((rule (car props)))
              (case (length rule)
                ('2 
                    (let* ((k (car rule))
                           (v (car (cdr rule))))
                        (update-model (cdr props)
                            (alist-set k v m))))
                ('3 
                    (let* ((op (car rule))
                           (k (car (cdr rule)))
                           (v (car (cdr (cdr rule)))))
                      (case op
                        ('push 
                            (let* ((old-list (alist-get k m))
                                  (new-list (cons v old-list)))
                                (update-model (cdr props)
                                          (alist-set k new-list m))))
                        ('pull m)
                        )))
                (else (console-log "unsuported update rule" rule)))))))
    
    (define (update-render src msg args)
        (let* ((m (model))
               (next (update src msg args m)))
            (console-log "update-render model" m)
            (console-log "update-render next" next)
            (case (list? next)
              ('#t 
                (case (length next)
                    ('0 (schedule-render))
                    ('1
                        (let ((m2 (update-model (car next) m)))
                            (console-log "update-render m2" m2)
                            (hashtable-set! state 'model m2) 
                            (schedule-render)))
                    ('2 
                        (let ((m2 (update-model (car next) m))
                              (cmds (car (cdr next))))
                            (console-log "update-render m2" m2)
                            (console-log "update-render cmds" cmds)
                            (hashtable-set! state 'model m2) 
                            (map send-cmd cmds)
                            (schedule-render)))
                    (else (console-error "invalid list length from update" next))))
              ('#f 
                (console-error "expected list from update" (list src msg args)
                (schedule-render))))))
          
    (define (render-attr attr)
      (let ((n (car attr))
            (v (cdr attr)))
        (case n 
          ((string "onclick") 
           (let ((fn (js-lambda (lambda () (update-render 'ui (car v) (cdr v))))))
             (list "onclick" fn )))
          ((string "onchange") 
           (let ((fn (js-lambda (lambda (args)
                                  (let* ((ev (car args))
                                         (value (js-ref (js-ref ev "target") "value")))
                                    (update-render 'ui (car v) value))))))
             (list "oninput" fn)))
          (else attr))))

    (define (render-elem elem) 
      (case (length elem)
        ('3 
         (let* ((tag (car elem))
                (args (cdr elem))
                (attrs (car args))
                (children (car (cdr args))))
           (h tag (map render-attr attrs) (map render-elem children))))
        ('2 (car (cdr elem)))
        (else (console-log "unknown html")))) 

    (define (render-view view)
      (case (list? view)
        ('#f (console-error "nothing to render" (model)))
        ('#t 
            (console-log "rendering view" view)
            (render-elem 
               (list "div" '() (list view))))))
    
    (define (subscribe-ws params) 
        (let* ((url (car params))
               (name (car (cdr params)))
               (conn (ws url)))
            (ws-on conn "onopen"
                      (lambda (socket)
                        (update-render name 'open '())))
            (ws-on conn "onclose"
                      (lambda ()
                        (update-render name 'close '())))
            (ws-on conn "onmessage"
                      (lambda (socket msg)
                        (let* ((str (js-ref msg 'data))
                               (rules (decode name))
                               (json-data (json-decode str))
                               (data2 (decode-first (list 'ok json-data) rules))
                               (decode-result (car data2)))
                          (case decode-result
                            ('ok 
                                (console-log "decode ok to" (car (cdr (cdr data2)))) 
                                (update-render name (car (cdr (cdr data2))) (car (cdr data2))))
                            (else (console-error "decoding error" 
                                                 (list data2 str)))))))
            (ws-on conn "onerror"
                   (lambda (msg)
                     (update-render name 'error '())))
            (list name (lambda (cmd)
                (console-log "sending cmd" cmd)
                (case (and (list? cmd) (= 1 (length cmd)))
                  ('#f (console-error "invalid cmd" cmd))
                  ('#t 
                    (let* ((msg (car cmd))
                           (data (encode name msg (model))))
                        (console-log "encoding to json" data)
                        (case (list? data)
                          ('#t 
                            (let ((str (json-encode (list->json data (js-obj)))))
                                (console-log "sending" str)
                                (js-invoke conn "send" str)))
                          ('#f (console-log "invalid json" data))))))))))
                     

    (define (subscribe sub)
        (let ((type (car sub))
              (params (cdr sub)))
          (case type
            ('websocket (add-sub (subscribe-ws params)))
            (else (console-log "unknown subscription" sub)))))
    
    (hashtable-set! state 'model (init)) 
    (hashtable-set! state 'subscriptions '()) 
    
    (mount-projector (lambda () (render-view (view (model)))))
    
    (map subscribe (subscriptions (hashtable-ref state 'model '())))
    ))
