(declare (mostly-fixnum)
	 (standard-bindings)
	 (extended-bindings)
	 (run-time-bindings)
	 (not safe))

(define-structure bitmap width height pixels)

(define ur-make-bitmap make-bitmap)
(define (make-bitmap w h)
  (ur-make-bitmap w h (make-u16vector (fx* w h) 0)))

(define (bitmap-plot bm x y color)
  (u16vector-set! (bitmap-pixels bm) (fx+ (fx* y (bitmap-width bm)) x) color))

(define (bitmap-sample bm x y)
  (u16vector-ref (bitmap-pixels bm) (fx+ (fx* y (bitmap-width bm)) x)))

(define (bitmap-display bm)
  (loop ((let: w (bitmap-width bm) h (bitmap-height bm))
         (for: y 0 (< y h) (+ y 1))
         (eval: (display "#|"))
         (after: (display "|#") (newline))
         (for: x 0 (< x w) (+ x 1))
         (when: (> x 0) (display " "))
         (when: (zero? (modulo x 4)) (display " "))
         (eval: (display (bitmap-sample bm x y))))))

(define (scale n max+1)
  "Return a value in [0,1] representing a scaling of N within [0, MAX]"
  (inexact->exact (round (fl* (min n 1.0) (fl- (fixnum->flonum max+1) 1.0)))))

(define (rgb->val r g b)
  "Return a 565-RGB value representing R, G, and B values in [0,1]"
  (fxbitwise-ior (fxarithmetic-shift (scale r 32) 11)
                 (fxarithmetic-shift (scale g 64) 5)
                 (scale b 32)))

(define pi (fl* 2.0 (flasin 1.0)))
(define pi/3 (fl/ pi 3.0))
(define pi*2 (fl* pi 2.0))

(define (mod2pi x)
  "Return X (a real) modulo pi times two"
  (let ((y (floor (fl/ x pi*2))))
    (fl- x (fl* pi*2 y))))

(define (mod2 x)
  "Return X (a real) modulo two"
  (let ((y (floor (fl/ x 2.0))))
    (fl- x (fl* 2.0 y))))

(define (hsv->val h s v)
  "Return a 565-RGB value representing an HSV color"
  (let* ((h (mod2pi h))
         (s (flmin 1.0 (flmax s 0.0)))
         (v (flmin 1.0 (flmax v 0.0)))
         (c (fl* v s))
         (h1 (fl/ h pi/3))
         (x (fl* c (fl- 1.0 (flabs (fl- (mod2 h1) 1.0)))))
         (m (fl- v c))
         (helper (lambda (r g b)
                   (rgb->val (fl+ r m) (fl+ g m) (fl+ b m)))))
    (cond ((fl< h1 1.0)
           (helper c x 0.0))
          ((fl< h1 2.0)
           (helper x c 0.0))
          ((fl< h1 3.0)
           (helper 0.0 c x))
          ((fl< h1 4.0)
           (helper 0.0 x c))
          ((fl< h1 5.0)
           (helper x 0.0 c))
          (else
           (helper c 0.0 x)))))
