(define (elementary-ws effect-name effect-settings send)
  
  (define (extract-values enc in out)
    (case (length enc)
      ('0 (list 'ok out))
      (else 
        (let* ((spec (car enc))
               (k (car spec))
               (value-spec (car (cdr spec)))
               (value (extract-value value-spec in)))
          (console-log "extracted value" value)
          (case (car value)
            ('ok (extract-values (cdr enc) in (set k (car (cdr value)) out)))
            (else value))))))
  
  (define (recv encs enc m) 
    (let ((encoded (extract-values enc m '())))
      (case (car encoded)
        ('ok (console-log "ws encoded" encoded))
        (else (console-error "decode error" (list enc m))))))

  (list 'ok recv))
