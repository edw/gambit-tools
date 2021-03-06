;;; LOOP
;;;
;;; (LOOP (OP ...) . BODY)
;;;
;;; Fundamental operators:
;;;
;;; (FOR: [NAME INITIAL STEP CONTINUE?] ...)
;;; (SEQ: [NAME LIST] ...)
;;; (SEQ#: [NAME VECTOR] ...)
;;; (WHILE: TEST)
;;; (IF: TEST)
;;; (EVAL: FORM ...)
;;; (LET: [NAME FORM] ...)
;;; (CONS: NAME KONS-PROC)
;;;
;;; and some shortcuts
;;;
;;; (WHEN: X FORM ...)       aka (EVAL: (if X (begin FORM ...)))
;;; (IF-NOT: X)              aka (IF: (not X))
;;; (UNTIL: X)               aka (WHILE (not X))
;;; (WHEN-NOT: X FORM ...)   aka (WHEN: (not X) FORM ...)

(define-macro (loop bindings . body)
  (let ((k (gensym 'loop-k)))
    `(call-with-current-continuation
      (lambda (,k)
        ,(let loop-impl ((bindings bindings) (body body))
           (if (null? bindings)
               (if (null? body)
                   `(values)
                   `(begin ,@body))
               (let* ((binding (car bindings))
                      (bindings (cdr bindings))
                      (op (car binding))
                      (op-args (cdr binding)))
                 (case op
                   ((cons:) (let ((x (car op-args))
                                  (kons (if (null? (cdr op-args))
                                            'cons (cadr op-args))))
                              (loop-impl
                               bindings
                               `((set! ,x (,kons (begin ,@body) ,x))))))
                   ((for:) (let ((iter (gensym 'loop-do))
                                 (x (car op-args))
                                 (first (cadr op-args))
                                 (test? (caddr op-args))
                                 (step (cadddr op-args))
                                 (rest (cddddr op-args)))
                             `(let ,iter ((,x ,first))
                                   (if ,test?
                                       (begin ,(if (null? rest)
                                                   (loop-impl bindings body)
                                                   (loop-impl `((for: ,@rest)
                                                                ,@bindings)
                                                              body))
                                              (,iter ,step))))))
                   ((seq:) (let ((iter (gensym 'loop-seq))
                                 (x (car op-args))
                                 (xs (gensym 'loop-xs))
                                 (initial-xs (cadr op-args))
                                 (rest (cddr op-args)))
                             `(let ,iter ((,xs ,initial-xs))
                                   (if (not (null? ,xs))
                                       (let ((,x (car ,xs)))
                                         ,(if (null? rest)
                                              (loop-impl bindings body)
                                              (loop-impl `((seq: ,@rest)
                                                           ,@bindings)
                                                         body))
                                         (,iter (cdr ,xs)))))))
                   ((seq#:) (let ((iter (gensym 'loop-seq-vector))
                                  (x (car op-args))
                                  (xs (gensym 'loop-xs))
                                  (i (gensym 'loop-i))
                                  (n (gensym 'loop-n))
                                  (initial-xs (cadr op-args))
                                  (rest (cddr op-args)))
                              `(let* ((,xs ,initial-xs)
                                      (,n (vector-length ,xs)))
                                 (let ,iter ((,i 0))
                                      (if (< ,i ,n)
                                          (let ((,x (vector-ref ,xs ,i)))
                                            ,(if (null? rest)
                                                 (loop-impl bindings body)
                                                 (loop-impl `((seq#: ,@rest)
                                                              ,@bindings)
                                                            body))
                                            (,iter (+ ,i 1))))))))
                   ((let:) (let ((x (car op-args))
                                 (value (cadr op-args))
                                 (rest (cddr op-args)))
                             `(let ((,x ,value))
                                ,(if (null? rest)
                                     (loop-impl bindings body)
                                     (loop-impl `((let: ,@rest) ,@bindings)
                                                body)))))
                   ((eval:) `(begin ,@op-args
                                       ,(loop-impl bindings body)))
                   ((after:) `(begin ,(loop-impl bindings body)
                                     ,@op-args))
                   ((while:) `(if ,(car op-args)
                                  ,(loop-impl bindings body)
                                  (,k)))
                   ((if:) `(if ,(car op-args)
                               ,(loop-impl bindings body)))
                   ((when:) (loop-impl `((eval:
                                          (if ,(car op-args)
                                              (begin ,@(cdr op-args))))
                                         ,@bindings)
                                       body))
                   ((if-not:) (loop-impl
                               `((filter: (not ,(car op-args)))
                                 ,@bindings)
                               body))
                   ((until:) (loop-impl
                              `((while: (not ,(car op-args))) ,@bindings)
                              body))
                   ((when-not:) (loop-impl
                                 `((when: (not ,(car op-args)) ,@(cdr op-args))
                                   ,@bindings)
                                 body))
                   (else (error "Unknown loop operator"))))))))))

;;; ARROWS

(define-macro (-> x . forms)
  (if (null? forms)
      x
      (let* ((form (car forms))
             (proc (car form))
             (args (cdr form))
             (forms (cdr forms)))
        `(-> (,proc ,x ,@args) ,@forms))))

(define-macro (--> x . forms)
  (if (null? forms)
      x
      (let* ((form (car forms))
             (proc (car form))
             (args (cdr form))
             (forms (cdr forms)))
        `(--> (,proc ,@args ,x) ,@forms))))

;;; BASHING ON OBJECTS

(define-macro (doto x . forms)
  (if (null? forms)
      x
      (let* ((form (car forms))
             (proc (car form))
             (args (cdr form))
             (forms (cdr forms)))
        `(begin (,proc ,x ,@args) (doto ,x ,@forms)))))
