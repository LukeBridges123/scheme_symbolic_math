#lang racket
#| A bunch of vector operations for vectors in R^N. Vectors are represented as lists of numbers.

|#
(require "../scheme_symbolic_math/mathematical-expressions.rkt")
(define make-vect list)
(define x-coord car)
(define y-coord cadr)
(define z-coord caddr)

(define (add-vect v w)
  (map make-sum v w))
;(add-vect '(1 2 3) '(3 2 1))
(define (sub-vect v w)
  (map make-difference v w))
;(sub-vect '(1 2 3) '(3 2 1))
(define (scale-vect s v)
  (map (lambda (x) (make-product s x)) v))
;(scale-vect 5 '(1 2 3))
(define (dot-product v w)
  (foldr make-sum
         0
         (map make-product v w)))
;(dot-product '(1 2 3) '(3 2 1))
(define (norm v)
  (make-exponentiation (dot-product v v) 1/2))
;including this helps reduce accumulation of numerical error + avoid redundant computation from calculating squared norm, taking its sqrt to get (norm v), then squaring it
;again later (e.g. in calculating an orthogonal projection)
(define (square-of-norm v)
  (dot-product v v))
;(norm '(3 4))
(define (orthogonal? v w)
  (= 0 (dot-product v w)))
;(orthogonal? '(3 0) '(0 2))
(define (angle-between v w)
  (acos (/ (dot-product v w)
           (* (norm v) (norm w)))))
;(angle-between '(3 0) '(0 2)) ;=pi/2 = 90 deg.

(define (proj-v-onto-w v w)
  (scale-vect (/ (dot-product v w)
                 (square-of-norm w))
              w))
;(proj-v-onto-w '(3 1 1) '(1 -1 0))

#|
computes the determinant of:
a b
c d
|#
(define (determinant-2x2 a b c d)
  (make-difference (make-product a d) (make-product b c)))

(define (cross-product vect-a vect-b)
  (let ((a1 (x-coord vect-a))
        (a2 (y-coord vect-a))
        (a3 (z-coord vect-a))
        (b1 (x-coord vect-b))
        (b2 (y-coord vect-b))
        (b3 (z-coord vect-b)))
    (make-vect (determinant-2x2 a2 a3 b2 b3)
               (make-product -1 (determinant-2x2 a1 a3 b1 b3))
               (determinant-2x2 a1 a2 b1 b2))))
(cross-product '(3 -1 1) '(1 2 -1))

(define (coeffs->plane-equation A B C D)
  (make-sum (make-sum (make-product A 'x)
                      (make-sum (make-product B 'y)
                                (make-product C 'z)))
            D))
;given a vector (A, B, C) perpendicular to the plane and a point in the plane, returns the expression
;Ax + By + Cz + D
;such that Ax + By + Cz + D = 0 defines the plane.
(define (normal+point->plane-equation normal-vector point-in-plane)
  (let ((A (x-coord normal-vector))
        (B (y-coord normal-vector))
        (C (z-coord normal-vector))
        (x0 (x-coord point-in-plane))
        (y0 (y-coord point-in-plane))
        (z0 (z-coord point-in-plane)))
    (coeffs->plane-equation A B C (* -1 (+ (* A x0) (* B y0) (* C z0))))))

(normal+point->plane-equation '(1 1 1) '(1 0 0))

(define (3-points->plane-equation point1 point2 point3)
  (let* ((vect1 (sub-vect point1 point2))
         (vect2 (sub-vect point1 point3))
         (normal-vector (cross-product vect1 vect2)))
    (normal+point->plane-equation normal-vector point1)))

(3-points->plane-equation '(1 1 1) '(2 0 0) '(1 1 0))
