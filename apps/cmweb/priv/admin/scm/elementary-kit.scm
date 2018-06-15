(define json (js-eval "JSON"))
(define (now) (js-invoke (js-new "Date") "getTime"))

(define (js-key k)
  (case (symbol? k)
    ('#t (symbol->string k))
    ('#f k)))

(define (js-val v)
  (case (list? v)
    ('#t
     (case (assoc? v)
       ('#t 
        (list->js v (js-obj)))
       ('#f 
        (list->js-array (map js-val v)))))
    (else 
     (case (symbol? v)
       ('#t (symbol->string v))
       (else v)))))
     
(define (list->js data r)
  (case (null? data)
    ('#t r)
    ('#f
     (case (list? data)
       ('#f (console-error "unexpected non-list data" data))
       ('#t 
         (case (assoc? data)
           ('#f (console-error "unexpected non-object data" data))
           ('#t 
             (let ((pair (car data)))
               (case (list? pair)
                 ('#t
                  (let* ((k (car pair))
                         (v (car (cdr pair))) 
                         (js-value (js-val v)))
                    (js-set! r (js-key k) js-value)
                    (list->js (cdr data) r)))
                 (else 
                    (console-error "unexpected non-pair value" pair)))))))))))

(define (assoc? in)
  (case (list? in)
    ('#f '#f)
    ('#t 
     (case (length in)
       ('0 '#t)
       (else
         (let ((first (car in)))
           (case (and (list? first)
                      (= 2 (length first))
                      (not (list? (car first))))
             ('#t (assoc? (cdr in)))
             (else '#f))))))))



(define (json-stringify source spaces) 
  (let ((r (list->js source (js-obj))))
    (js-invoke json "stringify" r '() spaces)))

(define (js-lambda fn) (js-closure (lambda args (apply fn (list args)))))

(define (js-defined? v) 
  (and (not (js-null? v)) (not (js-undefined? v))))

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

(define (infer-type v)
  (case (symbol? v)
    ('#t 'symbol)
    (else 
      (case (string? v)
        ('#t 'string)
        (else 
          (case (list? v)
            ('#t 'list)
            (else 
              (case (number? v)
                ('#t 'number)
                (else 
                  (case (boolean? v)
                    ('#t 'boolean)
                    (else 'other)))))))))))

(define (to-human-timestamp-since since millis)
  (let* ((secs (floor (/ (- since millis) 1000)))
         (secs-abs (abs secs))
         (ago (> secs 0)))
    (case (> secs-abs 60)
      ('#f (list 'seconds secs-abs ago))
      ('#t 
       (case (> secs-abs 3600)
         ('#f (list 'minutes (floor (/ secs-abs 60)) ago))
         ('#t 
          (case (> secs-abs 86400)
            ('#f (list 'hours (floor (/ secs-abs 3600)) ago))
            ('#t (list 'days (floor (/ secs-abs 86400)) ago)))))))))

(define (to-human-timestamp millis)
  (to-human-timestamp-since (now) millis))

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
  (case spec
    ('any (list 'ok '()))
    (else 
      (case (length spec)
        ('0 (list 'ok out))
        (else
          (let* ((entry-spec (car spec))
                 (k (car entry-spec))
                 (value-spec (car (cdr entry-spec)))
                 (encoded (encode value-spec input)))
            (case (car encoded )
              ('ok (encode-object (cdr spec) input (set k (car (cdr encoded)) out)))
              (else encoded))))))))

(define (encode-from spec input)
  (case (length input)
    ('0 (list 'error 'no-input input))
    (else 
      (case (symbol? spec)
        ('#t 
         (let* ((v (get spec input)))
           (case v
             ('undef (list 'error 'missing-key spec input))
             (else (list 'ok v)))))
        (else 
          (case (length spec)
            ('2 
             (let* ((key (car spec))
                    (spec2 (car (cdr spec)))
                    (encode2-fn (case (symbol? spec2)
                                  ('#t encode-from)
                                  ('#f encode)))
                    (input2 (encode2-fn spec2 input)))
               (case (car input2)
                 ('ok 
                  (encode-from key (car (cdr input2))))
                 (else
                   (list 'error 'encode-error spec2 input2)))))
            (else (console-error "unsupported encode spec " spec))))))))


(define (encode-map-match-options options in) 
  (case (length options)
    ('0 '(error no-options-matched))
    (else
      (let* ((opt (car options))
             (opt-source (get 'source opt))
             (opt-source-value (encode opt-source in)))
        (case (car opt-source-value)
          ('ok
           (case (eq? (car (cdr opt-source-value)) in)
             ('#f (encode-map-match-options (cdr options) in))
             (else 
              (let* ((opt-target (get 'target opt))
                     (opt-target-value (encode opt-target in)))
                (case (car opt-target-value)
                  ('ok (list 'ok (car (cdr opt-target-value))))
                  (else (list 'error encode-error opt-target)))))))
          (else (list 'error encode-error opt-source)))))))

(define (encode-map spec input)
  (let* ((value-spec (get 'value spec))
         (options-spec (get 'options spec))
         (v (encode value-spec input)))
    (case (car v)
      ('ok
       (let ((encoded (encode-map-match-options options-spec (car (cdr v)))))
         (case (car encoded)
           ('ok encoded)
           (else (console-error "unable to map value" (list spec input encoded))))))
      (else (console-error "unable to encode value in map spec" spec)))))

(define (encode-list specs in out)
  (case (length specs)
    ('0 (list 'ok (reverse out)))
    (else 
      (let* ((spec (car specs))
             (encoded (encode spec in)))
        (case (car encoded)
          ('ok (encode-list (cdr specs) in (cons (car (cdr encoded)) out)))
          (else 
            (console-error "cannot encode list" specs in encoded)))))))

(define (encode-number spec in)
  (case (number? spec)
    ('#t (list 'ok spec))
    ('#f 
     (console-error "invalid number spec" spec in)
     (list 'error 'invalid-number-spec spec in))))

(define (encode-text spec in)
  (case (string? spec)
    ('#t (list 'ok spec))
    (else 
      (let ((encoded (encode spec in)))
        (case (car encoded)
          ('ok (to-string (car (cdr encoded))))
          (else encoded))))))

(define (encode-maybe spec in)
  (let ((encoded (encode spec in)))
    (case (car encoded)
      ('ok encoded)
      (else '(ok ())))))

(define (encode-files spec in)
  (console-error "not supported" spec)
  (list 'error 'not-supported spec))

(define (encode-file spec in)
  (let ((encoded (encode spec in)))
    (case (car encoded)
      ('ok
       (let ((files (car (cdr encoded))))
         (case (and (list? files) (> (length files) 0))
           ('#t 
            (let* ((first (car files))
                   (f (get 'file first)))
              (case f 
                ('undef (list 'error 'invalid-file encoded))
                (else (list 'ok f)))))
           (else (list 'error 'not-a-list-of-files files)))))
      (else (list 'error 'invalid-file-spec spec)))))

(define (encode-either options in)
  (case (length options)
    ('0 (list 'error 'no-conditions-matched))
    (else 
      (let* ((pair (car options))
             (cond-spec (get 'condition pair))
             (value-spec (get 'spec pair))
             (cond-verified (eval-condition cond-spec in)))
        (case cond-verified
          ('#t 
           (let ((encoded-value (encode value-spec in)))
             (case (car encoded-value)
               ('ok (list 'ok (car (cdr encoded-value))))
               (else 
                 (console-error "cannot encode value from spec" pair)
                 (list 'error 'invalid-value-spec pair)))))
          ('#f (encode-either (cdr options) in)))))))

(define (encode-sum specs in)
  (fold-numbers + specs in 0))

(define (encode-percentage spec in)
  (let ((r (encode-ratio spec in)))
    (case (car r)
      ('ok 
       (list 'ok (floor (* 100 (car (cdr r))))))
      (else r))))

(define (encode-ratio spec in)
  (let ((num-spec (get 'num spec))
        (den-spec (get 'den spec)))
    (case num-spec
      ('undef (console-error "invalid ratio spec" spec))
      (else 
        (case den-spec
          ('undef (console-error "invalid ratio spec" spec))
          (else 
            (let ((num (encode num-spec in))
                  (den (encode den-spec in)))
              (case (car num)
                ('ok
                 (case (car den)
                   ('ok (list 'ok (/ (car (cdr num)) (car (cdr den)))))
                   (else den)))
                (else num)))))))))

(define (fold-numbers fn specs in out)
  (case (length specs)
    ('0 (list 'ok out))
    (else 
      (let* ((next (car specs))
             (encoded (encode next in)))
        (case (car encoded)
          ('ok
           (let ((v (car (cdr encoded))))
             (case (number? v)
               ('#t (fold-numbers fn (cdr specs) in (fn v out)))
               (else (list 'error not-a-number encoded)))))
          (else encoded))))))


(define (fold-lists fn specs in out)
  (case (length specs)
    ('0 (list 'ok out))
    (else 
      (let* ((spec (car specs))
             (encoded (encode spec in)))
        (case (car encoded)
          ('ok
           (let ((v (car (cdr encoded))))
            (case (list? v)
              ('#t (fold-lists fn (cdr specs) in (fn out v)))
              (else (list 'error 'not-a-list encoded)))))
          (else encoded))))))

(define (encode-merge specs in)
  (fold-lists append specs in '()))

(define (encode-format spec in)
  (let* ((pattern (get 'pattern spec))
         (params (get 'params spec))
         (encoded-pattern (encode pattern spec in))
         (encoded-params (encode-list params in '())))
    (case (car encoded-pattern)
      ('ok
       (case (car encoded-params) 
         ('ok
          (list 'ok (apply format (cons (car (cdr encoded-pattern)) (car (cdr encoded-params))))))
         (else (list 'error cannot-encode-params spec in))))
      (else (list 'error cannot-encode-pattern spec in)))))

(define (encode spec input) 
  (case (list? spec)
    ('#f
     (case (or (symbol? spec) (string? spec) (number? spec))
       ('#t (list 'ok spec))
       ('#f 
        (console-error "invalid spec" spec)
        (list 'error 'invalid-spec spec))))
    ('#t 
      (let ((type-spec (car spec))
            (value-spec (car (cdr spec))))
        (case type-spec
          ('symbol (list 'ok value-spec))
          ('text (encode-text value-spec input))
          ('format (encode-format value-spec input))
          ('number (encode-number value-spec input))
          ('from (encode-from value-spec input))
          ('object (encode-object value-spec input '()))
          ('list (encode-list value-spec input '()))
          ('map (encode-map value-spec input))
          ('maybe (encode-maybe value-spec input))
          ('files (encode-files value-spec input))
          ('file (encode-file value-spec input))
          ('either (encode-either value-spec input))
          ('sum (encode-sum value-spec input))
          ('ratio (encode-ratio value-spec input))
          ('percentage (encode-percentage value-spec input))
          ('merge (encode-merge value-spec input))
          (else
            (console-error "invalid value spec" spec)
            '(error invalid-spec)))))))

(define (decode-boolean spec in)
  (case (and (boolean? in) (or (eq? 'any spec) (eq? in spec)))
    ('#t (list 'ok in))
    ('#f (list 'error 'boolean-mismatch spec in))))

(define (decode-symbol spec in)
  (case (and (symbol? in) (eq? in spec))
    ('#t (list 'ok in))
    ('#f (list 'error 'symbol-mismatch spec in))))

(define (decode-text spec in)
  (let* ((condition (and (string? in) (or (eq? 'any spec) (eq? in spec)))))
    (case condition
      ('#t (list 'ok in))
      ('#f (list 'error 'text-mismatch spec in)))))

(define (decode-number spec in)
  (case (and (number? in) (or (eq? 'any spec) (eq? in spec)))
    ('#t (list 'ok in))
    ('#f (list 'error 'number-mismatch spec in))))

(define (decode-v-spec spec)
   (case (list? spec)
     ('#f (list (inspect-basic-type spec) spec))
     ('#t (list (car spec) (car (cdr spec))))))

(define (decode-term spec in)
  (case in 
    ('undef '(error undef))
    (else 
      (let* ((decoded-spec (decode-v-spec spec))
             (type-spec (car decoded-spec))
             (value-spec (car (cdr decoded-spec))))
        (case type-spec
          ('symbol (decode-symbol value-spec in))
          ('text (decode-text value-spec in))
          ('number (decode-number value-spec in))
          ('object (decode-object value-spec in '()))
          ('list (decode-list value-spec in)) 
          ('boolean (decode-boolean value-spec in))
          ('entries (decode-entries value-spec in '()))
          ('one_of (decode-one-of value-spec in))
          ('maybe (decode-maybe value-spec in))
          ('data (decode-data value-spec in))
          (else
            (console-error "unsupported decoder spec type" spec type-spec value-spec in)
            '(error invalid-type-spec)))))))

(define (decode-data spec in)
  (case spec
    ('any (list 'ok in))
    (else 
      (console-error "unsupported data decoder spec" spec in)
      (list 'error 'unsupported-data-decoder-spec))))

(define (decode-maybe spec in)
  (let ((decoded (decode-term spec in)))
    (case (car decoded)
      ('ok decoded)
      (else (list 'ok (default-for spec ))))))

(define (default-for spec)
  (case (car spec)
    ('text "")
    ('number 0)
    ('symbol 'undef)
    ('boolean '#f)
    (else '())))

(define (decode-one-of specs in)
  (case (length specs)
    ('0 (list 'error 'one-of-none-matched in))
    (else 
      (let* ((spec (car specs))
             (decoded (decode-term spec in)))
        (case (car decoded)
          ('ok decoded)
          (else (decode-one-of (cdr specs) in)))))))

(define (decode-entries spec in out)
  (case (length in)
    ('0 (list 'ok out))
    (else 
      (let ((next (car in)))
        (case (and (list? next) (eq? 2 (length next)))
          ('#t 
           (let* ((k (car next))
                  (v (car (cdr next)))
                  (decoded (decode-term spec v)))
             (case (car decoded)
               ('ok (decode-entries spec (cdr in) (cons (list (list 'key k)
                                                              (list 'value (car (cdr decoded)))) out)))
               (else 
                 (console-error "error decoding entry" decoded)
                 decoded)))) 
          ('#f (list 'error 'invalid-entry next)))))))

(define (decode-list spec in)
  (case spec 
    ('any
     (case (or (list? in) (= 0 (length in)))
       ('#t (list 'ok in))
       (else (list 'error 'not-a-list in))))
    (else 
      (case (number? spec)
        ('#t (decode-list-size spec in))
        ('#f 
         (case (length in)
           ('0 (list 'error 'list-size-mismatch (length in)))
           (else (decode-non-empty-list spec in '()))))))))

(define (decode-list-size size in)
  (case (eq? size (length in))
    ('#t (list 'ok in))
    ('#f (list 'error 'list-size-mismatch size in))))

(define (decode-non-empty-list spec in out)
  (case (length in)
    ('0 (list 'ok (reverse out)))
    (else
      (case spec
        ('any (decode-non-empty-list spec (cdr in) (cons (car in) out)))
        (else 
          (case (car spec)
            ('object (decode-items decode-object! (car (cdr spec)) in out))
            ('text (decode-items decode-text (car (cdr spec)) in out))
            ('file (decode-items decode-file (car (cdr spec)) in out))
            (else 
              (console-error "unsupported list decoder spec type" spec)
                '(error invalid--list-type-spec))))))))

(define (decode-file spec in)
  (list 'ok (list (list 'name (js-ref in "name"))
                  (list 'size (js-ref in "size"))
                  (list 'type (js-ref in "type"))
                  (list 'modified (js-ref in "lastModified"))
                  (list 'file in))))

(define (decode-items fn spec in out)
  (case (length in)
    ('0 (list 'ok (reverse out)))
    (else 
      (let* ((next (car in))
             (decoded (fn spec next)))
        (case (car decoded)
          ('ok (decode-items fn spec (cdr in) (cons (car (cdr decoded)) out)))
          (else decoded))))))

(define (decode-object! spec in)
    (decode-object spec in '()))

(define (decode-object spec in out)
  (case in 
    ('undef '(error not-an-object in))
    (else
      (case (assoc? in)
        ('#f '(error not-an-object in))
        ('#t 
          (case spec
            ('any (list 'ok in))
            (else 
              (case (length spec)
                ('0 (list 'ok out))
                (else 
                  (let* ((entry-spec (car spec))
                         (k (car entry-spec))
                         (v-spec (car (cdr entry-spec)))
                         (decoded (decode-term v-spec (get k in))))
                    (case (car decoded)
                      ('ok (decode-object (cdr spec) in (set k (car (cdr decoded)) out)))
                      (else decoded))))))))))))

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

(define (eval-condition spec in)
  (case (car spec)
    ('true '#t)
    ('false '#f)
    ('equal (all-equal? (car (cdr spec)) in 'undef))
    ('is_set
     (let ((encoded (encode (car (cdr spec)) in)))
       (eq? 'ok (car encoded))))
    ('not (not (eval-condition (car (cdr spec)) in)))
    (else 
      (console-error "unsupported condition" spec)
      '#f)))

(define (all-equal? exprs in expected) 
  (case (length exprs)
    ('0 '#t)
    (else 
      (let* ((next (car exprs))
             (encoded (encode next in)))
        (case (car encoded)
          ('ok 
           (let ((v (car (cdr encoded))))
             (case (or (eq? 'undef expected) (eq? v expected))
               ('#t (all-equal? (cdr exprs) in v))
               ('#f '#f))))
          (else 
            (console-error "cannot eval condition " next encoded exprs in expected)
            '#f))))))

(define (to-string v)
  (let ((infered-type (infer-type v)))
    (case infered-type
      ('symbol (list 'ok (symbol->string v)))
      ('string (list 'ok v))
      ('number (list 'ok (number->string v)))
      ('list 
       (case (length v)
         ('0 (list 'ok ""))
         (else (list 'ok (string-append (map to-string v))))))
      ('boolean
       (case v
         ('#t (list 'ok "true"))
         ('#f (list 'ok "false"))))
      (else (console-error "can't convert value into a string" infered-type v)))))

