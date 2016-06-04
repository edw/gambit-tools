;;;; Gambit iteration tools
;;;; Edwin Watkeys, edw@poseur.com
;;;; 04 June 2016

;; (for ((a '(1 2 3))
;;       (b '(1 10 100))
;;       (let: ((pi (* 4 (atan 1)))))
;;       (when: (= b 10))
;;       (while: (< b 90)))
;;   (* 2 a b pi))

;; (loop ((a '(1 2 3))
;;        (b '(1 10 100))
;;        (let: ((pi (* 4 (atan 1)))))
;;        (when: (= b 10))
;;        (while: (< b 90)))
;;   (display (* 2 a b pi))
;;   (newline))

;;; (for ((a 4 0 (atan 1))) a)

(define (cursor col . start+step)
  (cond
   ((number? col)
    (let ((i 0)
          (count col)
          (start (if (null? start+step)
                     0
                     (car start+step)))
          (step (if (or (null? start+step)
                        (null? (cdr start+step)))
                    1
                    (cadr start+step))))
      (letrec ((proc (lambda (method)
                       (cond ((equal? method 'value)
                              (if (< i count) start #f))
                             ((equal? method 'next)
                              (if (< i count)
                                  (begin (set! start (+ start step))
                                         (set! i (+ i 1))))
                              proc)
                             ((equal? method 'null?)
                              (>= i count))))))
        proc)))
   ((list? col)
    (letrec ((proc
              (lambda (method)
                (cond ((equal? method 'value)
                       (if (null? col)
                           #f
                           (car col)))
                      ((equal? method 'next)
                       (if (not (null? col))
                           (set! col (cdr col)))
                       proc)
                      ((equal? method 'null?)
                       (null? col))))))
      proc))
   ((vector? col)
    (let ((index 0)
          (count (vector-length col)))
      (letrec ((proc
                (lambda (method)
                  (cond ((equal? method 'value)
                         (if (< index count)
                             (vector-ref col index)
                             #f))
                        ((equal? method 'next)
                         (if (< index count)
                             (set! index (+ index 1)))
                         proc)
                        ((equal? method 'null?)
                         (not (< index count)))))))
        proc)))
   (else (error "Unsupported type"))))

(define (cursor-value c) (c 'value))
(define (cursor-next! c) (c 'next))
(define (cursor-null? c) (c 'null?))

(define (bindings->pairs bindings)
  (let loop ((bs bindings) (pairs '()))
    (cond ((null? bs)
           (reverse pairs))
          ((null? (cdr bs))
           (error "Odd number of values in bindings"))
          (else
           (loop (cddr bs) (cons (list (car bs) (cadr bs)) pairs))))))

(define (for-impl bindings form iter in out)
  (cond ((null? bindings)
         `(,iter (cursor-next! ,in) (cons ,form ,out)))
        (else
         (let ((key (caar bindings))
               (val (cadar bindings))
               (vals (cdar bindings))
               (rest (cdr bindings)))
           (cond ((eq? key while:)
                  `(if ,val
                       ,(for-impl rest form iter in out)
                       (reverse ,out)))
                 ((eq? key when:)
                  `(if ,val
                       ,(for-impl rest form iter in out)
                       (,iter (cursor-next! ,in) ,out)))
                 ((eq? key let:)
                  `(let ,val
                     ,(for-impl rest form iter in out)))
                 (else
                  (let ((new-iter (gensym 'for-iter))
                        (new-in (gensym 'for-in))
                        (new-out (gensym 'for-out)))
                    `(let ,new-iter ((,new-in (cursor ,@vals))
                                     (,new-out ,out))
                          (if (cursor-null? ,new-in)
                              (,iter (cursor-next! ,in) ,new-out)
                              (let ((,key (cursor-value ,new-in)))
                                ,(for-impl rest form
                                           new-iter new-in new-out)))))))))))

(define-macro (for bindings form)
  (for-impl bindings form
            '(lambda (_ out) (reverse out))
            '(cursor '())
            ''()))

(define (loop-impl bindings forms iter in)
  (cond ((null? bindings)
         `(begin
            ,@forms
            (,iter (cursor-next! ,in))))
        (else
         (let ((key (caar bindings))
               (val (cadar bindings))
               (vals (cdar bindings))
               (rest (cdr bindings)))
           (cond
            ((eq? key while:)
             `(if ,val
                  ,(loop-impl rest forms iter in)))
            ((eq? key when:)
             `(if ,val
                  ,(loop-impl rest forms iter in)
                  (,iter (cursor-next! ,in))))
            ((eq? key let:)
             `(let ,val
                ,(loop-impl rest forms iter in)))
            (else
             (let ((new-iter (gensym 'for-iter))
                   (new-in (gensym 'for-in)))
               `(let ,new-iter ((,new-in (cursor ,@vals)))
                     (if (cursor-null? ,new-in)
                         (,iter (cursor-next! ,in))
                         (let ((,key (cursor-value ,new-in)))
                           ,(loop-impl rest forms new-iter new-in)))))))))))

(define-macro (loop bindings . forms)
  (loop-impl bindings forms
             '(lambda (_) (values))
             '(cursor '())))

;; (for ((a '(1 2 3))
;;       (b '(1 10 100))
;;       (let: ((pi (* 4 (atan 1)))))
;;       (when: (= b 10))
;;       (while: (< b 90)))
;;   (* 2 a b pi))

;; (loop ((a '(1 2 3))
;;        (b '(1 10 100))
;;        (let: ((pi (* 4 (atan 1)))))
;;        (when: (= b 10))
;;        (while: (< b 90)))
;;   (display (* 2 a b pi))
;;   (newline))

;;; (for ((a 4 0 (atan 1))) a)
