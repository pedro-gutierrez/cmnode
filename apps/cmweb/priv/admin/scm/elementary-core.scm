(define (app init update decoders encoders effects) 
  (let ((state (make-eq-hashtable))
        (effects-registry (make-eq-hashtable)))
    
    (define (model) (hashtable-ref state 'model '()))
    
    (define (load-effects effs registry)
        (case (length effs)
          ('0 
           (console-log "all effects loaded")
           registry)
          (else 
            (let ((eff (car effs)))
              (case (length eff)
                ('3
                 (let ((eff-name (car eff))
                       (eff-class (car (cdr eff)))
                       (eff-settings (car (cdr (cdr eff)))))
                 (load-effect eff-name eff-class eff-settings registry)))
                (else (console-error "invalid effect" eff)))
              (load-effects (cdr effs) registry)))))
    
    (define (effect-url eff-class eff-settings)
        (let* ((url (get 'effect-url eff-settings)))
          (case url 
            ('undef (string-append "scm/" (symbol->string eff-class) ".scm"))
            (else url))))

    (define (load-effect eff-name eff-class eff-settings registry) 
        (let* ((eff-url (effect-url eff-class eff-settings)))
            (load eff-url)
            (let ((eff-send (apply (eval eff-class) (list eff-name eff-settings effect-recv)))
                  (eff-state (make-eq-hashtable)))
              (hashtable-set! eff-state 'send (car (cdr eff-send)))
              (hashtable-set! registry eff-name eff-state)
              (console-log "loaded effect" eff-name))))  
    
    (define (effect eff-name) (map-get eff-name effects-registry)) 

    (define (effect-send eff encs enc m)
      (let ((s (hashtable-ref eff 'send '())))
        (s encs enc m)))

    (define (effect-recv data)
      (console-log "received" data))
    

    (define (load-encoders encs registry)
        (case (length encs)
          ('0
           (console-log "all encoders loaded")
           registry)
          (else
            (let ((enc (car encs)))
              (case (length enc)
                ('2
                 (let ((enc-name (car enc))
                       (enc-spec (car (cdr enc))))
                   (load-encoders (cdr encs) (load-encoder enc-name enc-spec registry))))
                (else (console-error "invalid encoder" enc)))))))
    
    (define (load-encoder enc-name enc-spec registry)
      (set enc-name enc-spec registry))

    (define (encoders-registry)(map-get 'encoders state))
    
    (define (encode spec m)
        spec)

    (define (apply-cmd spec m)
      (console-log "applying cmd" spec)
      (case (length spec)
        ('2 
         (let* ((eff-name (car spec))
                (enc-name (car (cdr spec)))
                (encs (encoders-registry))
                (eff (effect eff-name))
                (enc (get enc-name encs)))
           (case eff
             ('undef (console-error "no such effect" eff-name))
             (else 
               (case enc
                 ('undef (console-error "no such encoder" enc-name))
                 (else (effect-send eff encs enc m)))))))
        (else (console-error "invalid cmd" spec))))

    (define (apply-cmds cmds m)
      (console-log "applying cmds" cmds)
      (case (length cmds)
        ('0 
         (console-log "all cmds done")
         m)
        (else 
          (apply-cmd (car cmds) m)
          (apply-cmds (cdr cmds) m))))
    
    (define (apply-model-spec spec msg m) 
      (case (length spec)
        ('0 m)
        (else 
          (let* ((rule (car spec))
                 (k (car rule))
                 (v (car (cdr rule))))
            (apply-model-spec (cdr spec) msg (set k v m))))))

    (define (apply-update spec msg m)
      (case (length spec)
        ('2 
         (let ((m2 (apply-model-spec (car spec) msg m)))
           (apply-cmds (car (cdr spec)) m2)))
        (else (console-error "invalid update spec" spec))))
        
    (load-effects (effects) effects-registry)
    (hashtable-set! state 'encoders (load-encoders (encoders) '()))
    
    ;(effect-send 'ui (list "button" '(("onclick" "test")) (list (list "it works baby"))))
    
    (hashtable-set! state 'model (apply-update (init) '() '())) 

    (apply (eval 'console-log) (list "it works"))))
