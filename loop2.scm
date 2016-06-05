;;; LOOP
;;;
;;; (LOOP (OP ...) . BODY)
;;;
;;; Fundamental operators:
;;;
;;; (FOR: NAME INITIAL STEP CONTINUE?)
;;; (SEQ: NAME LIST)
;;; (SEQ#: NAME VECTOR)
;;; (WHILE: TEST)
;;; (IF: TEST)
;;; (PERFORM: FORM ...)
;;; (LET: NAME FORM)
;;; (COLLECT: NAME)
;;;
;;; and some shortcuts
;;;
;;; (WHEN: X FORM ...)       aka (PERFORM: (if X (begin FORM ...)))
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
                   ((for:) (let* ((iter (gensym 'loop-do))
                                 (x (car op-args))
                                 (first (cadr op-args))
                                 (test? (caddr op-args))
                                 (step (cadddr op-args)))
                             `(let ,iter ((,x ,first))
                                  (if ,test?
                                      (begin ,(loop-impl bindings body)
                                             (,iter ,step))))))
                   ((seq:) (let ((iter (gensym 'loop-seq))
                                 (x (car op-args))
                                 (xs (gensym 'loop-xs))
                                 (initial-xs (cadr op-args)))
                             `(let ,iter ((,xs ,initial-xs))
                                   (if (not (null? ,xs))
                                       (let ((,x (car ,xs)))
                                         ,(loop-impl bindings body)
                                         (,iter (cdr ,xs)))))))
                   ((seq#:) (let ((iter (gensym 'loop-seq-vector))
                                  (x (car op-args))
                                  (xs (gensym 'loop-xs))
                                  (i (gensym 'loop-i))
                                  (n (gensym 'loop-n))
                                  (initial-xs (cadr op-args)))
                              `(let* ((,xs ,initial-xs)
                                      (,n (vector-length ,xs)))
                                 (let ,iter ((,i 0))
                                      (if (< ,i ,n)
                                          (let ((,x (vector-ref ,xs ,i)))
                                            ,(loop-impl bindings body)
                                            (,iter (+ ,i 1))))))))
                   ((let:) `(let ((,(car op-args) ,(cadr op-args)))
                              ,(loop-impl bindings body)))
                   ((perform:) `(begin ,@op-args
                                       ,(loop-impl bindings body)))
                   ((while:) `(if ,(car op-args)
                                  ,(loop-impl bindings body)
                                  (,k)))
                   ((if:) `(if ,(car op-args)
                                   ,(loop-impl bindings body)))
                   ((when:) (loop-impl `((perform:
                                          (if ,(car op-args)
                                              (begin ,@(cdr op-args)))
                                          ,@bindings))
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

#|

(define (loop-fizzbuzz)
  (loop ((seq: x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (when: (zero? (modulo x 3)) (display "Fizz") (newline))
         (when: (zero? (modulo x 5)) (display "Buzz") (newline)))))

(define (loop-until)
  (loop ((seq: x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (if: (even? x))
         (until: (> x 7)))
    (display x) (newline)))

(define (loop-cons)
  (let ((out '()))
    (loop ((for: a 0.0 (< a 5) (+ a (atan 1)))
           (seq: x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
           (let: y (+ x a))
           (cons: out))
      y)
    (reverse out)))

(define (loop-seq)
  (let ((out '()))
    (loop ((seq: x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
           (cons: out))
      x)
    (reverse out)))

(define (loop-let)
  (let ((out '()))
    (loop ((let: x 42)
           (cons: out))
      x)
    (reverse out)))

(define (loop-for)
  (let ((out '()))
    (loop ((for: a 0.0 (< a 5) (+ a (atan 1)))
           (cons: out))
      a)
    (reverse out)))

(define (loop-cons-simple)
  (let ((out '()))
    (loop ((seq: x '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
           (cons: out))
      x)
    (reverse out)))

|#
