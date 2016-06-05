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

(load "loop-impl.scm")

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
