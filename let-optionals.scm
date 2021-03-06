;;; LET-OPTIONALS

(define-macro (let-optionals ARGS BINDINGS . BODY)
  (if (null? BINDINGS)
      `(if (null? ,ARGS)
           (let () ,@BODY)
           (error "Too many arguments"))
      (let ((BINDING (car BINDINGS))
            (BINDINGS (cdr BINDINGS)))
        (let ((NAME (car BINDING))
              (DEFAULT (if (null? (cdr BINDING))
                           (error "Missing default value")
                           (cadr BINDING)))
              (TEST (if (null? (cddr BINDING))
                        #t
                        (caddr BINDING)))
              (ARGS-NAME (gensym 'ARGS))
              (NEXT-ARGS-NAME (gensym 'NEXT-ARGS)))
          `(let* ((,ARGS-NAME ,ARGS)
                  (,NEXT-ARGS-NAME (if (null? ,ARGS-NAME)
                                       '()
                                       (cdr ,ARGS-NAME)))
                  (,NAME (if (null? ,ARGS-NAME)
                             #f (car ,ARGS)))
                  (,NAME (cond ((null? ,ARGS-NAME)
                                ,DEFAULT)
                               (,TEST
                                ,NAME)
                               (else ,DEFAULT))))
             (let-optionals ,NEXT-ARGS-NAME ,BINDINGS ,@BODY))))))

;;; LET-OPTIONALS*

(define-macro (let-optionals* ARGS BINDINGS . BODY)
  (if (null? BINDINGS)
      `(begin ,@BODY)
      (let ((BINDING (car BINDINGS))
            (BINDINGS (cdr BINDINGS)))
        (if (and (not (pair? BINDING)) (null? BINDINGS))
            `(let ((,BINDING ,ARGS))
               ,@BODY)
            (let ((NAME (car BINDING))
                  (DEFAULT (if (null? (cdr BINDING))
                               (error "Missing default value")
                               (cadr BINDING)))
                  (TEST (if (null? (cddr BINDING))
                            #t
                            (caddr BINDING)))
                  (ARGS-NAME (gensym 'ARGS))
                  (NEXT-ARGS-NAME (gensym 'NEXT-ARGS)))
              `(let* ((,ARGS-NAME ,ARGS)
                      (,NEXT-ARGS-NAME (if (null? ,ARGS-NAME)
                                           '()
                                           (cdr ,ARGS-NAME)))
                      (,NAME (if (null? ,ARGS-NAME)
                                 #f (car ,ARGS)))
                      (,NAME (cond ((null? ,ARGS-NAME)
                                    ,DEFAULT)
                                   (,TEST
                                    ,NAME)
                                   (else ,DEFAULT))))
                 (let-optionals* ,NEXT-ARGS-NAME ,BINDINGS ,@BODY)))))))
