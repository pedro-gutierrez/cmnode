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

    (define (render-attr attr)
        (let ((n (car attr))
              (v (cdr attr)))
          (case n
            ((string "onclick")
             (let ((fn (js-lambda (lambda () (send-event (car v) (cdr v))))))
               (list "onclick" fn )))
            ((string "onchange")
             (let ((fn (js-lambda (lambda (args)
                                    (let* ((ev (car args))
                                           (value (js-ref (js-ref ev "target") "value")))
                                      (send-event (car v) value))))))
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
        ('2
         (let ((kind (car elem))
               (value (car (cdr elem))))
           (case kind
             ('view
              (let ((v (get value (views))))
                (case v 
                  ('undef (console-error "unknown view" value))
                  (else (render-elem v)))))
             ('from (model-value value))
             (else (console-log "unknown directive" elem)))))
        ('1 (car elem))
        (else (console-log "unknown html"))))
    
    (define (default-view)
      (list "div" '() (list (list "nothing to render"))))
    
    (define (views) (hashtable-ref state 'views '()))
    (define (model) (hashtable-ref state 'model '()))
    
    (define (model-value k) 
      (let ((v (get k (model))))
        (case v
          ('undef "")
          (else v))))

    (define (view) 
      (let ((v (hashtable-ref state 'view '())))
        (case (null? v)
          ('#t (default-view))
          ('#f v))))

    (define (render-view)(render-elem (list "div" '() (list (view)))))

    (define (recv encs enc m)
      (hashtable-set! state 'view enc)
      (hashtable-set! state 'views encs)
      (hashtable-set! state 'model m)
      (schedule-render))
    
    (mount-projector (lambda () (render-view)))

    (list 'ok recv)))
