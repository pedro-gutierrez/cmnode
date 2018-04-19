(define (app init update decoders encoders effects) 
  (let ((state (make-eq-hashtable))
        (effects-registry (make-eq-hashtable))
        (decoders-registry (make-eq-hashtable))
        (updates-registry (make-eq-hashtable)))
    
    (define (model) (hashtable-ref state 'model '()))
    
    (define (load-updates upds registry)
      (case (length upds)
        ('0 registry)
        (else 
            (let ((upd (car upds)))
              (case (length upd)
                ('2
                 (let* ((upd-event (car upd))
                        (upd-spec (car (cdr upd))))
                   (case (length upd-spec)
                     ('2 (load-updates (cdr upds) (load-update upd-event upd-spec registry)))
                     (else (console-error "invalid update spec" upd-spec )))))
                (else (console-error "invalid update" upd)))))))
    
    (define (load-update upd-event upd-spec registry)
      (set upd-event upd-spec registry))
    
    (hashtable-set! state 'updates (load-updates (update) '()))
    
    (define (try-decoders decs data)
      (case (or (eq? 'undef decs) (= 0 (length decs)))
        ('#t '(error no-decoder))
        (else
          (let* ((dec (car decs))
                 (msg (car dec))
                 (spec (car (cdr dec)))
                 (decoded (decode spec data)))
            (case (car decoded)
              ('ok (list 'ok msg (car (cdr decoded))))
              (else (try-decoders (cdr decs) data)))))))

    (define (decode-data data)
      (case (list? data)
        ('#f '(error bad-format))
        ('#t 
         (let ((eff (get 'effect data)))
           (case eff
             ('undef '(error no-effect))
             (else (try-decoders (map-get eff decoders-registry) data)))))))

    (define (effect-recv data)
      (console-log "decoding" data)
      (let ((decoded (decode-data data)))
        (console-log "decoded" decoded)
        (case (car decoded)
          ('ok 
           (let* ((msg (car (cdr decoded)))
                  (data (car (cdr (cdr decoded))))
                  (update-spec (update-for msg)))
             (case update-spec
               ('undef (console-error "no such update spec" msg))
               (else (apply-update update-spec data (model))))))
          (else (console-error "no decoder for" data)))))
    
    (define (load-decoders decs registry)
      (case (length decs)
        ('0 registry)
        (else 
            (let ((dec (car decs)))
              (case (length dec)
                ('2
                 (let* ((dec-event (car dec))
                        (dec-spec (car (cdr dec)))
                        (spec-type (car dec-spec))
                        (spec (car (cdr dec-spec)))
                        (dec-effect (get 'effect spec) ))
                   (case dec-effect
                     ('undef (console-error "invalid decoder (missing effect)" dec))
                     (else (load-decoders (cdr decs) (load-decoder dec-effect dec registry))))))
                (else (console-error "invalid decoder" dec)))))))
    
    (define (load-decoder dec-effect dec registry)
      (map-push-at dec-effect dec registry))
    
    
    (define (update-for upd-event)
      (let ((specs (map-get 'updates state)))
        (case specs 
          ('undef (console-error "no updates loaded"))
          (else (get upd-event specs)))))

    (define (load-effects effs registry)
        (case (length effs)
          ('0 
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
              (hashtable-set! registry eff-name eff-state))))  
    
    (define (effect eff-name) (map-get eff-name effects-registry)) 

    (define (effect-send eff encs enc m)
      (let ((s (hashtable-ref eff 'send '())))
        (s encs enc m)))
    

    (define (load-encoders encs registry)
        (case (length encs)
          ('0 registry)
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
    
    (define (apply-cmd spec m)
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
                 (else
                   (effect-send eff encs enc m)))))))
        (else (console-error "invalid cmd" spec))))

    (define (apply-cmds cmds m)
      (case (length cmds)
        ('0 
         (hashtable-set! state 'model m)
         m)
        (else 
          (apply-cmd (car cmds) m)
          (apply-cmds (cdr cmds) m))))
   
    (define (apply-model-spec spec msg m) 
      (case (length spec)
        ('0 (list 'ok m))
        (else
          (let* ((rule (car spec))
                 (k (car rule))
                 (value-spec (car (cdr rule)))
                 (resolved (encode value-spec msg)))
            (case (car resolved) 
              ('ok (apply-model-spec (cdr spec) msg (set k (car (cdr resolved)) m)))
              (else '(error bad-spec)))))))

    (define (apply-update spec msg m)
      (case (length spec)
        ('2 
         (let* ((model-spec (car spec))
                (m2 (apply-model-spec model-spec msg m)))
           (console-log "update" spec)
           (case (car m2)
             ('ok (apply-cmds (car (cdr spec)) (car (cdr m2))))
             (else (console-error "invalid update spec" model-spec)))))
        (else (console-error "invalid update spec" spec))))
        
    (load-effects (effects) effects-registry)
    (load-decoders (decoders) decoders-registry)
    (hashtable-set! state 'encoders (load-encoders (encoders) '()))
    (hashtable-set! state 'model (apply-update (init) '() '())) ))
