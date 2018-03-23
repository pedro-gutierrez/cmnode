;
;
;
(define (decode-all data rules)
  (case (null? rules)
    ('#t data)
    ('#f (decode-all (decode-one data (car rules)) (cdr rules)))))
;
;
;
(define (decode-one data rule)
  (console-log "decode-one" data)
  (console-log "decode-one" rule)
  (case (car rule)
    ('proc (decode-proc data (car (cdr rule))))
    ('match (decode-match data (car (cdr rule))))
    ('one-of (decode-first data (car (cdr rule))))))

;
;
;
(define (decode-proc prev proc)
  (list 'ok (proc (car (cdr prev)))))


;
;
;
;
(define (decode-first prev rules)
  (case (null? rules)
    ('#t '(error nomatch))
    ('#f 
        (let* ((first (car rules))
               (next (decode-one prev first)))
          (case (car next)
            ('ok next)
            (else (decode-first prev (cdr rules))))))))

; Eg:
;
; '(match ((action "connect")
;          (data ((session string)))) connect)
;
;
(define (decode-match data rules)
  (case (car data)
    ('ok (decode-match-props (car (cdr data)) rules '()))
    (else '(error data))))

(define (decode-match-props data rules result)
  (case (null? rules)
    ('#t (list 'ok result))
    ('#f 
        (let* ((rule (car rules))
               (key (car rule))
               (value (car (cdr rule)))
               (js-key (js-ref data (symbol->string key))))
          (console-log "decode-match-props - rule" rule)
          (console-log "decode-match-props - key" key)
          (console-log "decode-match-props - value" value)
          (console-log "decode-match-props - js-key" js-key)
          (case (or (js-undefined? js-key) (js-null? js-key))
            ('#t 
                (console-log "decode-match-props missing" key) 
                (list 'error (list 'missing key)))
            ('#f
                (case (symbol? value)
                    ('#t 
                        (console-log "decode-match-props parsing type" js-key)
                        (case value
                          ('text (decode-match-props data
                                    (cdr rules) (alist-set key js-key result)))
                          ('string (decode-match-props data
                                    (cdr rules) (alist-set key js-key result)))
                          ('number 
                            (let ((num (string->number js-key)))
                              (case (number? num)
                                ('#t (decode-match-props data
                                    (cdr rules) (alist-set key num result)))
                                ('#f '(error wrong-value)))))
                          (else '(error wrong-value-type))))
                    ('#f 
                        (case (string? value)
                          ('#t 
                            (case (eq? value js-key)
                              ('#t (decode-match-props data 
                                    (cdr rules) (alist-set key value result)))
                              ('#f '(error wrong-value)))) 
                          ('#f 
                            (case (list? value) 
                              ('#t 
                                (console-log "decode-match-props list")
                                (let ((nested (decode-match-props js-key value '())))
                                    (case (car nested)
                                        ('error nested)
                                        ('ok
                                            (console-log "decode-match-props nested" nested)
                                            (decode-match-props data
                                    (cdr rules) (alist-set key (car (cdr nested)) result))))))
                              ('#f 
                                (console-log "decode-match-props unknown rule " value)
                                '(error wrong-value-type)))))))))))))

 (console-log (decode-all '(ok "{ \"action\": \"connect\", \"data\": { \"session\": \"a123\" } }")
   (list (list 'proc json-decode)
         (list 'one-of 
               '((match ((action "connect")
                         (data ((session number))))))))))
