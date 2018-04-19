(define (set k v m)
  (cons (list k v) m))

(define (get k m)
  (let ((entry (assoc k m)))
    (case entry
      ('#f 'undef)
      (else (car (cdr entry))))))
    
(define (map-get k m)
  (case (hashtable-contains? m k)
      ('#f 'undef)
      ('#t (hashtable-ref m k '()))))

(define (map-push-at k v m)
  (let ((v2 (map-get k m)))
    (case v2 
      ('undef 
       (hashtable-set! m k '())
       (map-push-at k v m))
      (else
        (hashtable-set! m k (cons v v2))
        m)))) 

(define (encode-object spec input out)
  (case (length spec)
    ('0 (list 'ok out))
    (else
      (let* ((entry-spec (car spec))
             (k (car entry-spec))
             (value-spec (car (cdr entry-spec)))
             (encoded (encode value-spec input)))
        (case (car encoded )
          ('ok (encode-object (cdr spec) input (set k (car (cdr encoded)) out)))
          (else encoded))))))

(define (encode spec input) 
  (case (list? spec)
    ('#f
     (case (or (symbol? spec) (string? spec))
       ('#t (list 'ok spec))
       ('#f 
        (console-error "invalid spec" spec)
        '(error invalid-spec))))
    ('#t 
      (let ((type-spec (car spec))
            (value-spec (car (cdr spec))))
        (case type-spec
          ('symbol (list 'ok value-spec))
          ('text (list 'ok value-spec))
          ('from 
           (let* ((v (get value-spec input)))
             (case v 
               ('undef (list 'error 'missing-key value-spec input))
               (else (list 'ok v)))))
          ('object (encode-object value-spec input '()))
          (else
            (console-error "invalid value spec" spec)
            '(error invalid-spec)))))))

(define (decode-symbol spec in)
  (case (and (symbol? in) (eq? in spec))
    ('#t (list 'ok in))
    ('#f '(error text-mismatch))))

(define (decode-text spec in)
  (case (and (string? in) (or (eq? 'any spec) (eq? in spec)))
    ('#t (list 'ok in))
    ('#f '(error text-mismatch))))

(define (decode-number spec in)
  (case (and (number? in) (or (eq? 'any spec) (eq? in spec)))
    ('#t (list 'ok in))
    ('#f '(error number-mismatch))))

(define (decode-v-spec spec)
   (case (list? spec)
     ('#f (list (inspect-basic-type spec) spec))
     ('#t (list (car spec) (car (cdr spec))))))

(define (decode-term spec in)
  (let* ((decoded-spec (decode-v-spec spec))
         (type-spec (car decoded-spec))
         (value-spec (car (cdr decoded-spec))))
    (case type-spec
      ('symbol (decode-symbol value-spec in))
      ('text (decode-text value-spec in))
      ('number (decode-number value-spec in))
      ('object (decode-object value-spec in '()))
      (else
        (console-error "unsupported decoder spec type" spec)
        '(error invalid-type-spec)))))
        
(define (decode-object spec in out)
  (case (length spec)
    ('0 (list 'ok out))
    (else 
      (let* ((entry-spec (car spec))
             (k (car entry-spec))
             (v-spec (car (cdr entry-spec)))
             (decoded (decode-term v-spec (get k in))))
        (case (car decoded)
          ('ok (decode-object (cdr spec) in (set k (car (cdr decoded)) out)))
          (else decoded))))))

(define (decode spec in)
  (case (car spec)
    ('object (decode-object (car (cdr spec)) in '()))
    (else
      (console-error "unsupported decode spec" spec)
      '(error unknown-spec))))


(define (inspect-basic-type v) 
  (case (symbol? v)
    ('#t 'symbol)
    ('#f
     (case (string? v) 
       ('#t 'text)
       ('#f
        (case (number? v)
          ('#t 'number)
          (else 
            (console-error "unsupported basic type" v))))))))


(define (js-lambda fn) (js-closure (lambda args (apply fn (list args)))))
