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
    
    (define (views) (hashtable-ref state 'encoders '()))
    (define (model) (hashtable-ref state 'model '()))
    

    (define (resolve-encoder name)
      (get name (views)))

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
      (case (and (list? spec) (> (length spec) 0))
        ('#f (list 'ok '()))
        (else (encode spec in))))
    
    (define (resolve-view-name spec ctx) 
      (case (symbol? spec)
        ('#t (list 'ok spec))
        (else 
          (let ((encoded (encode spec ctx)))
            (case (car encoded)
              ('ok encoded)
              (else (list 'error 'encode-error spec)))))))

    (define (resolve-view spec ctx)
      (let ((view-name (resolve-view-name (get 'name spec) ctx)))
        (case (car view-name)
          ('ok
            (let ((v (get (car (cdr view-name)) (views))))
              (case v
                ('undef (list 'error 'no-view view-name))
                (else (list 'ok v)))))
          (else (console-error "cannot resolve view name" view-name ctx)))))   

    (define (eval-view-condition spec in)
      (case spec
        ('undef '#t)
        (else (eval-condition spec in))))

    (define (compile-view-ref spec ctx)
      (let* ((params-spec (get 'params spec))
             (condition-spec (get 'condition spec))
             (v-ctx (view-ctx params-spec ctx)))
        (case (car v-ctx)
          ('ok
           (let ((v (resolve-view spec ctx)))
            (case (car v)
              ('ok 
               (let* ((view-params (car (cdr v-ctx)))
                      (condition-verified (eval-view-condition condition-spec view-params)))
                 (case condition-verified
                   ('#t (compile-view (car (cdr v)) view-params))
                   ('#f ""))))
              (else (console-error "no such view" spec)))))
          (else (console-error "cannot encode view context" spec ctx v-ctx)))))
    
    (define (compile-views spec ctx)
      (let ((v (resolve-view spec ctx)))
        (case (car v)
          ('ok
           (let ((v-ctx (list (list 'context (get 'context ctx)))))
               (let ((items (encode (get 'items spec) ctx))
                     (item-view (car (cdr v))))
                 (case (car items)
                   ('ok (map (lambda (item)
                               (let ((v-ctx2 (set 'item item v-ctx)))
                                 (compile-view item-view v-ctx2)))  (car (cdr items)))) 
                   (else (console-error "unable to convert spec into a list of items" spec))))))
          (else (console-error "no such view" spec)))))
    
    (define (attr-name attr)
      (case (symbol? attr)
        ('#t (symbol->string attr))
        ('#f attr)))
    
    (define (compile-value spec ctx)
      (case (list? spec)
        ('#t 
          (case (car spec)
            ('encoder
             (let ((enc (resolve-encoder (car (cdr spec)))))
               (case enc
                 ('undef (console-error "no such encoder" (list encode-spec)))
                 (else (encode enc ctx))))) 
            (else (encode spec ctx))))
        ('#f (encode spec ctx))))

    (define (compile-attr attr ctx)
      (let ((n (attr-name (car attr)))
            (v (compile-value (car (cdr attr)) ctx)))
        (case (car v)
          ('ok
           (case n
             ((string "onclick")
              (let ((fn (js-lambda (lambda (args) (send-event (car (cdr v)) '())))))
                (list "onclick" fn )))
             ((string "onchange")
              (let ((fn (js-lambda (lambda (args)
                                     (let* ((ev (car args))
                                            (target (js-ref ev "target"))
                                            (ev-value (map-event-value target)))
                                       (send-event (car (cdr v)) ev-value))))))
                (list "oninput" fn)))
             (else (list n (car (cdr v))))))
          (else 
            (console-error "error encoding attribute value" v)
            v))))

    (define (map-event-value target)
      (let ((files (js-ref target "files")))
        (case (js-defined? files)
          ('#t 
           (js-array->list files))
          ('#f (js-ref target "value")))))

    (define (compile-json-view v ctx)
      (let ((source (encode (get 'source v) ctx))
            (indent (encode (get 'indent v) ctx)))
        (case (car source)
          ('ok
           (case (car indent)
             ('ok
                (json-stringify (car (cdr source)) (car (cdr indent))))
             (else (console-error "cannot encode json view indent value" v indent))))
          (else 
            (console-error "cannot encode json view source" v source)))))

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
             ('text 
              (let ((encoded (encode v ctx)))
                (case (car encoded)
                  ('ok (car (cdr encoded)))
                  (else (console-error "can't encode text" encoded)))))
             ('view (compile-view-ref value ctx))
             ('from
              (let ((encoded (encode v ctx)))
                (case (car encoded)
                  ('ok (cdr encoded))
                  (else (console-error "unable to compile text" encoded)))))
             ('json (compile-json-view value ctx))
             (else (console-error "unknown directive" v)))))
        ('1 v)
        (else (console-error "unknown view" v ))))

    (define (update-view encs enc) 
      (case enc
        ('nil 'ok)
        (else 
          (hashtable-set! state 'view enc)
          (hashtable-set! state 'encoders encs))))


    (define (recv encs enc m)
      (hashtable-set! state 'model m)
      (update-view encs enc)
      (schedule-render))
    
    (mount-projector (lambda () (render-view)))

    (list 'ok recv)))
