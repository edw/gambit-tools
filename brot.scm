(declare (mostly-fixnum)
	 (standard-bindings)
	 (extended-bindings)
	 (not safe))

(include "loop2.scm")
(include "bitmap.scm")

(define color-black 0)

(define max-iters 256)

(define (cell-color cr ci)
  (do ((zr 0.0 (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr))
       (zi 0.0 (fl+ (fl* 2.0 zr zi) ci))
       (i 0 (fx+ i 1)))
      ((or (fx= i max-iters)
	   (fl> (fl+ (fl* zr zr) (fl* zi zi)) (flsquare 2.0)))
       (let ((iters-norm (fl/ (fixnum->flonum (fx- max-iters i))
			      (fixnum->flonum max-iters))))
	 (hsv->val (fl* pi*2 iters-norm) 1.0 iters-norm)))))

(define brot-width 8)
(define brot-height 8)

(define brot-bitmap (make-bitmap brot-width brot-height))

(define (brot cx cy scale theta)
  (loop ((let: cx-pxs (fl/ (fl- (fixnum->flonum brot-width) 1.0) 2.0)
               cy-pxs (fl/ (fl- (fixnum->flonum brot-height) 1.0) 2.0)
               sin-theta (flsin theta)
               cos-theta (flcos theta))
         (for: y 0 (< y brot-height) (fx+ y 1))
         (let: i0 (fl* scale (fl- (fixnum->flonum y) cy-pxs)))
         (for: x 0 (< x brot-width) (fx+ x 1))
         (let: j0 (fl* scale (fl- (fixnum->flonum x) cx-pxs))
               i1 (fl- (fl* cos-theta i0) (fl* sin-theta j0))
               j1 (fl+ (fl* cos-theta j0) (fl* sin-theta i0))
               i (fl+ cy i1)
               j (fl+ cx j1)))
    (bitmap-plot brot-bitmap brot-width brot-height x y (cell-color i j)))
  brot-bitmap)

;; (brot 6.77636708120639e-4 -1.57497160008726 5.0e-1 0.0)

