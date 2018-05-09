(app 
  
  ;; init 
  (lambda () '((state connecting)
               (images ())))
  
  ;; view 
  (lambda (model)
    (case (alist-get 'state model)
      ('"plop"
       (list "div" '()
             (list (list "p" '(("class" "default"))
                         (list
                           (list "button" '(("onclick" "test")
                                            ("class" "btn"))
                                 (list (list "text" "message")))
                           (list "text" "default"))))))
      
      ('signing-in
        '( "p" () (("text" "signing in..."))))
      
      ('connected
        '( "p" () (("text" "connected"))))
      
      ('connecting
        '( "p" () (("text" "connecting..."))))
        
      ('login
        (list "div" '(("key" "login")
                      ("class" "column col-6 col-md-12 col-xs-12"))
            (list 
              (list "h1" '() (list (list "text" "We koneKt you to the world")))
              (user-message (alist-get 'user-message model))
              '("div" (("class" "form-group")) 
                ( ("label" (("class" "form-label")
                            ("for" "email")) 
                    (("text" "Email")) )
                  ("input" (("class" "form-input")
                            ("type" "text")
                            ("id" "email")
                            ("onchange" email)
                            ("placeholder" "Email")) () )))
             
              '("div" (("class" "form-group")) 
                ( ("label" (("class" "form-label")
                            ("for" "password")) 
                    (("text" "Password")) )
                  ("input" (("class" "form-input")
                            ("type" "password")
                            ("id" "password")
                            ("onchange" password)
                            ("placeholder" "Password")) () )))

              '("div" (("class" "form-group")) 
                ( ("button" (("class" "btn")
                             ("onclick" login))
                    (("text" "Login")) ))))))

      ('home 
        (list "div" '(("key" "search")
                      ("class" "column col-12 col-md-12 col-xs-12"))
            (list 
            (list "h1" '() (list (list "text" "Search"))) 
                  (user-message (alist-get 'user-message model))
                  '("div" (("class" "form-group"))
                    ( ("label" (("class" "form-label")
                                ("for" "keywords"))
                       (("text" "Keywords")) )
                     ("input" (("class" "form-input")
                               ("type" "text")
                               ("id" "keywords")
                               ("onchange" keyword)
                               ("placeholder" "Keywords")) () )))

                  '("div" (("class" "form-group")) 
                    ( ("button" (("class" "btn")
                                 ("onclick" search))
                       (("text" "Search pictures")) )))
                  
                  (images-panel model)  
                  
                  )))
                  
       ))
  
  
  ;; decode
  (lambda (src)
    (case src
      ('ws '((connect ((action "connect")
                       (status "ok")
                       (data ((session text)))))
             (image ((action "image")
                     (status "ok")
                     (data ((title text)
                            (url text)
                            (id text)
                            (date ((year text)
                                   (month text)
                                   (day text)
                                   (hour text)
                                   (min text)))))))
             (error ((status "error")))
             (not-implemented ((action text)
                               (status "not_implemented")))
             (missing-input ((action text)
                             (status "invalid")
                             (data ((missing text))))) 

             ))))

  ;; encode
  (lambda (dst msg model)
    (case dst 
      ('ws 
       (case msg 
         ('search
            (list (list 'action 'search)
                  (list 'keyword (alist-get 'keyword model))))
         ('login (list (list 'action 'login)
                       (list 'username (alist-get 'email model))
                       (list 'password (alist-get 'password model))))))))

  ;; update  
  (lambda (src msg data model)
    (case src
      ('ws 
        (case msg
          ('open '(((state connected))))
          ('connect '(((state login))))
          ('image
            (list (list (list 'push 'images (alist-get 'data data)))))
          ('close '())
          ('error '())
          ('not-implemented
           (list (list (list 'user-message 
                             (string-append "Not implemented: " (alist-get 'action data))))))
          ('missing-input
            (case (alist-get 'state model)
              ('signing-in
                (list 
                    (list (list 'state 'login)
                          (list 'user-message "Check your data and try again" ))))
              (else '())))))

      ('ui 
        (case msg
          ('email (list (list (list 'email data))))
          ('password (list (list (list 'password data))))
          ('keyword (list (list (list 'keyword data))))
          ;('login 
          ;  '(((state signing-in))
          ;    ((ws login))))
          ('login 
            '(((state home))))
          ('search
            '(((images ())(image undef))((ws search))))
          ('select-image 
            (list (list (list 'image (car data)))))
       ))))
  
  ;; subscriptions
  (lambda (model)
     (list 
       (list 'websocket server_url 'ws))))


; View partials
;
(define (user-message msg)
  (case (string? msg)
    ('#f '("div" (("id" "user-message")) ()))
    ('#t (list "div" '(("id" "user-message")) (list (list "text" msg))))))

(define (images-panel model)
  (let ((img (alist-get 'image model)))
    (case img
        ('undef
            (list "div" '(("class" "container"))
                (list
                  (list "div" '(("class" "columns"))
                    (map image-thumb (alist-get 'images model))))))
        (else 
          (list "div" '(("class" "container"))
                (list 
                  (list "div" '(("class" "columns"))
                        (list 
                          (list "div" '(("class" "col-6")
                                        ("key" "image-preview"))
                                (list (image-preview img)))
                          (list "div" '(("class" "col-6")
                                        ("key" "image-actions"))
                                (list (list)))))))))))

(define (image-thumb img)
  (list "div" (list (list "class" "col-3 col-xs-12 col-md-6 c-hand")
                    (list "onclick" 'select-image img)
                    (list "key" (alist-get "id" img)))
        (list 
          (image-preview img))))
        
(define (image-preview img)
    (console-log "rendering img" img)
    (list "div" '(("class" "caption"))
        (list 
            (list "img" (list 
                          (list "class" "img-responsive")
                          (list "src" (alist-get 'url img))) '())
            (list "figcaption" '(("class" "figure-caption text-center"))
                  (list (list "text" (alist-get 'title img)))))))

