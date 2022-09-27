#lang scheme
(require "mathematical-expressions.rkt")
(require "symbolic-differentiation.rkt")

#|
Finds the indefinite integral of expr with respect to var. Leaves out the usual ambiguous constant.
|#
(define (indefinite-integral expr var)
  (cond
    ((number? expr) (make-product expr var))
    ((same-variable? expr var) (make-product (/ 1 2) (make-exponentiation var 2)))
    ((sum? expr) (make-sum (indefinite-integral (addend expr))
                           (indefinite-integral (augend expr))))
    ((and (product? expr) (number? (multiplier expr))) (make-product (multiplier expr)
                                                                     (indefinite-integral (multiplicand expr) var)))
    ((and (exponentiation? expr) (eq? (base expr) var) (number? (power expr)))
     (make-product (/ 1 (+ (power expr) 1))
                   (make-exponentiation var (+ (power expr) 1))))
    (else (error "Unknown expression type: INDEFINITE-INTEGRAL" expr))))
(define (exact-definite-integral expr var a b)
  (let ((antiderivative (indefinite-integral expr var)))
    (- (evaluate-expr antiderivative var b)
       (evaluate-expr antiderivative var a))))
(indefinite-integral '(5 * (x ^ 3)) 'x)
(indefinite-integral '(2 * x) 'x)
(exact-definite-integral '(x ^ 2) 'x 0 1)