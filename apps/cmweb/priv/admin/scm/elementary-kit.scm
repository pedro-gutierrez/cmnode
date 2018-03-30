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
    
(define (extract-value spec input) 
  (case (or (symbol? spec) (string? spec))
    ('#t (list 'ok spec))
    ('#f 
     (case (list? spec)
       ('#t
        (case (car spec)
          ('from 
           (let* ((k (car (cdr spec)))
                  (v (get k input)))
             (case v 
               ('undef '(error no-value))
               (else (list 'ok v))))) 
          (else '(error invalid-spec))))
       ('#f '(error invalid-spec))))))
