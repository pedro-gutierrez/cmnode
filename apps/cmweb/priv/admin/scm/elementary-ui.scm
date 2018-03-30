(define (elementary-ui effect-name effect-settings send)
  (let ((state (make-eq-hashtable)))
    
    (define doc (js-eval "document"))
    (define body (js-ref doc "body"))
    (define maquette (js-eval "maquette"))
    (define projector (js-invoke maquette "createProjector" ))
    (define h* (js-ref maquette "h"))
    (define (js-lambda fn) (js-closure (lambda args (apply fn (list args)))))

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

    (define (attr-value spec ctx)
      (case (list? spec)
        ('#f spec)
        ('#t
         (case (length spec)
           ('2 
            (case (car spec)
              ('from (ctx-value (car (cdr spec)) ctx))
              (else (console-error "attribute spec not supported yet" spec))))
           (else (console-error "invalid attribute spec" spec))))))

    (define (render-attr attr ctx)
        (let ((n (attr-name (car attr)))
              (v (attr-value (car (cdr attr)) ctx)))
          (case n
            ((string "onclick")
             (let ((fn (js-lambda (lambda (args) (send-event v '())))))
               (list "onclick" fn )))
            ((string "onchange")
             (let ((fn (js-lambda (lambda (args)
                                    (let* ((ev (car args))
                                           (value (js-ref (js-ref ev "target") "value")))
                                      (send-event v value))))))
               (list "oninput" fn)))
            (else (list n v)))))
      
    (define (render-elem elem ctx)
;      (console-log "rendering elem" elem)
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
             ('from (ctx-value value ctx))
             (else (console-error "unknown directive" elem)))))
        ('1 (car elem))
        (else (console-error "unknown html"))))
     
    (define (merged-ctx ctx params) 
      (case params
        ('undef ctx)
        (else
          (case (length params)
            ('0 ctx)
            (else 
              (let* ((first (car params))
                     (name (car first))
                     (val (car (cdr first))))
                (merged-ctx (set name val ctx) (cdr params))))))))

    (define (render-subview spec ctx)
      (let ((view-name (get 'name spec)))
        (case view-name
          ('undef (console-error "invalid subview spec" spec))
          (else
            (let ((subview (get view-name (views))))
              (case subview
                ('undef (console-error "unknown view" view-name))
                (else 
                  (render-elem subview (merged-ctx ctx (get 'params spec))))))))))

    (define (default-view)
      (list "div" '() (list (list "nothing to render"))))
    
    (define (views) (hashtable-ref state 'views '()))
    (define (model) (hashtable-ref state 'model '()))
    
    (define (ctx-value k ctx) 
      (let ((v (get k ctx)))
        (case v
          ('undef "")
          (else v))))

    (define (view) 
      (let ((v (hashtable-ref state 'view '())))
        (case (null? v)
          ('#t (default-view))
          ('#f v))))

    (define (render-view)(render-elem (list "div" '() (list (view))) (model)))

    (define (recv encs enc m)
      (hashtable-set! state 'view enc)
      (hashtable-set! state 'views encs)
      (hashtable-set! state 'model m)
      (schedule-render)
      )
    
    (mount-projector (lambda () (render-view)))

    (list 'ok recv)))
