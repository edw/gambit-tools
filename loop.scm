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

(define-macro (for bindings form)
  (include "loop-impl.scm")
  (for-impl bindings form
            '(lambda (_ out) (reverse out))
            '(cursor '())
            ''()))

(define-macro (loop bindings . forms)
  (include "loop-impl.scm")
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
