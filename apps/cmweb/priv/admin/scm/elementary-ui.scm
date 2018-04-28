(define (elementary-ui effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))
    
    (define doc (js-eval "document"))
    (define body (js-ref doc "body"))
    (define maquette (js-eval "maquette"))
    (define projector (js-invoke maquette "createProjector" ))
    (define h* (js-ref maquette "h"))

    (define (encode-attrs attrs obj)
      (case (length attrs)
        ('0 obj)
        (else 
          (let* ((attr (car attrs)))
            (js-set! obj (car attr) (car (cdr attr)))
            (encode-attrs (cdr attrs) obj)))))

    (define (h elem attrs children)
        (let ((attrs2 (encode-attrs attrs (js-obj)))
              (children2 (list->js-array children)))
          (js-call h* elem attrs2 children2)))

    (define (mount-projector fn)
        (js-invoke projector "append" body (js-lambda fn)))

    (define (schedule-render)
        (js-invoke projector "scheduleRender"))
    
    (define (send-event ev-name ev-value)
      (case (null? ev-value)
        ('#t (send (list (list 'effect effect-name)
                         (list 'event ev-name))))
        ('#f  (send (list (list 'effect effect-name)
                          (list 'event ev-name)
                          (list 'value ev-value))))))


      
     
    (define (default-view)
      (list "div" '() (list 'list (list (list 'text "nothing to render")))))
    
    (define (views) (hashtable-ref state 'views '()))
    (define (model) (hashtable-ref state 'model '()))
    
    (define (view) 
      (let ((v (hashtable-ref state 'view '())))
        (case (null? v)
          ('#t (default-view))
          ('#f v))))

    (define (render-view)
      (let ((v (compile-view (list "div" '() (list 'list (list (view)))) (model))))
        (render-elem v)))
    
    (define (render-elem elem)
      (case (list? elem)
        ('#f elem)
        ('#t 
          (case (length elem)
            ('3
             (let* ((tag (car elem))
                    (args (cdr elem))
                    (attrs (car args))
                    (children (car (cdr args))))
               (h tag attrs (map render-elem children))))
            ('1 (car elem))))))
    
    (define (view-ctx spec in) 
      (case spec
        ('undef (list 'ok '()))
        (else (encode spec in))))
    
    (define (resolve-view spec)
      (let ((view-name (get 'name spec)))
        (case view-name
          ('undef (console-error "invalid subview spec" spec))
          (else
            (let ((v (get view-name (views))))
              (case v
                ('undef (list 'error 'no-view view-name))
                (else (list 'ok v))))))))
    
    (define (compile-view-ref spec ctx)
      (let ((v (resolve-view spec)))
        (case (car v)
          ('ok  
           (let ((v-ctx (view-ctx (get 'params spec) ctx)))
            (case (car v-ctx)
              ('ok (compile-view (car (cdr v)) (car (cdr v-ctx))))
              (else (console-error "invalid context for subview" (list spec v-ctx))))))
          (else (console-error "no such view" spec)))))
    
    (define (compile-views spec ctx)
      (console-log "compile-views" spec)
      (let ((v (resolve-view spec)))
        (case (car v)
          ('ok
           (let ((v-ctx (view-ctx (get 'params spec) ctx)))
            (case (car v-ctx)
              ('ok
               (let ((items (encode (get 'items spec) ctx))
                     (item-view (car (cdr v))))
                 (case (car items)
                   ('ok (map (lambda (item)
                               (let ((v-ctx2 (set 'item item (car (cdr v-ctx)))))
                                (compile-view item-view v-ctx2)))  (car (cdr items)))) 
                   (else (console-error "unable to convert spec into a list of items" spec)))))
              (else (console-error "invalid context for subview" (list spec v-ctx))))))
          (else (console-error "no such view" spec)))))
    
    (define (attr-name attr)
      (case (symbol? attr)
        ('#t (symbol->string attr))
        ('#f attr)))
    
    (define (compile-attr attr ctx)
      (let ((n (attr-name (car attr)))
            (v (encode (car (cdr attr)) ctx)))
        (case (car v)
          ('ok
           (case n
             ((string "onclick")
              (let ((fn (js-lambda (lambda (args) (send-event (car (cdr v)) '())))))
                (list "onclick" fn )))
             ((string "onchange")
              (let ((fn (js-lambda (lambda (args)
                                     (let* ((ev (car args))
                                            (value (js-ref (js-ref ev "target") "value")))
                                       (send-event (car (cdr v)) value))))))
                (list "oninput" fn)))
             (else (list n (car (cdr v))))))
          (else 
            (console-error "error encoding attribute value" v)
            v))))
   
    (define (compile-view v ctx)
      (case (length v)
        ('3
         (let* ((tag (car v))
                (args (cdr v))
                (attrs (car args))
                (children-spec (car (cdr args)))
                (compile-attr-fn (lambda (a) (compile-attr a ctx))))
           (case (car children-spec)
             ('list
                (list tag (map compile-attr-fn attrs) 
                      (map (lambda (c) (compile-view c ctx)) (car (cdr children-spec)))))
             ('loop
                (list tag (map compile-attr-fn attrs) (compile-views (car (cdr children-spec)) ctx )))
             (else
               (console-error "unsupported children spec" children-spec)))))
        ('2
         (let ((kind (car v))
               (value (car (cdr v))))
           (case kind
             ('text value )
             ('view (compile-view-ref value ctx))
             ('from
              (let ((encoded (encode v ctx)))
                (case (car encoded)
                  ('ok (cdr encoded))
                  (else (console-error "unable to compile text" encoded)))))
             ;('iterate (compile-views value ctx)) 
             (else (console-error "unknown directive" v)))))
        ('1 v)
        (else (console-error "unknown view" v ))))

    (define (update-view encs enc) 
      (case enc
        ('nil 'ok)
        (else 
          (hashtable-set! state 'view enc)
          (hashtable-set! state 'views encs))))


    (define (recv encs enc m)
      (hashtable-set! state 'model m)
      (update-view encs enc)
      (schedule-render))
    
    (mount-projector (lambda () (render-view)))

    (list 'ok recv)))
