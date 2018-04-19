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

    (define (attr-name attr)
      (case (symbol? attr)
        ('#t (symbol->string attr))
        ('#f attr)))

    (define (render-attr attr ctx)
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
      
    (define (render-elem elem ctx)
      (case (length elem)
        ('3
         (let* ((tag (car elem))
                (args (cdr elem))
                (attrs (car args))
                (children (car (cdr args)))
                (render-attr-fn (lambda (a) (render-attr a ctx)))
                (render-child-fn (lambda (c) (render-elem c ctx))))
           (h tag (map render-attr-fn attrs) (map render-child-fn children))))
        ('2
         (let ((kind (car elem))
               (value (car (cdr elem))))
           (case kind
             ('view (render-subview value ctx))
             ('from 
              (let ((encoded (encode elem ctx)))
                (case (car encoded)
                  ('ok (car (cdr encoded)))
                  (else (console-error "unable to render text" encoded))))) 
             (else (console-error "unknown directive" elem)))))
        ('1 (car elem))
        (else (console-error "unknown html"))))
     
    (define (subview-ctx spec in) 
      (case spec
        ('undef (list 'ok '()))
        (else (encode spec in))))

    (define (render-subview spec ctx)
      (let ((view-name (get 'name spec)))
        (case view-name
          ('undef (console-error "invalid subview spec" spec))
          (else
            (let ((subview (get view-name (views))))
              (case subview
                ('undef (console-error "unknown view" view-name))
                (else
                  (let ((sub-ctx (subview-ctx (get 'params spec) ctx)))
                    (case (car sub-ctx)
                      ('ok
                       (render-elem subview (car (cdr sub-ctx))))
                      (else 
                        (console-error "invalid context for subview" (list spec sub-ctx))
                        sub-ctx))))))))))

    (define (default-view)
      (list "div" '() (list (list "nothing to render"))))
    
    (define (views) (hashtable-ref state 'views '()))
    (define (model) (hashtable-ref state 'model '()))
    
    (define (view) 
      (let ((v (hashtable-ref state 'view '())))
        (case (null? v)
          ('#t (default-view))
          ('#f v))))

    (define (render-view)(render-elem (list "div" '() (list (view))) (model)))
    
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
