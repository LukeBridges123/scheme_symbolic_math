#lang racket
(require "mathematical-expressions.rkt")
(require "symbolic-differentiation.rkt")

#|
Finds the indefinite integral of expr with respect to var. Leaves out the usual ambiguous constant.
|#
(define (indefinite-integral expr var)
  (cond
    ((number? expr) (make-product expr var))
    ((same-variable? expr var) (make-product (/ 1 2) (make-exponentiation var 2)))
    ((sum? expr) (make-sum (indefinite-integral (addend expr) var)
                           (indefinite-integral (augend expr) var)))
    ((difference? expr) (make-difference (indefinite-integral (minuend expr) var)
                                         (indefinite-integral (subtrahend expr) var)))
    ((and (product? expr) (number? (multiplier expr)))
     (make-product (multiplier expr)
                   (indefinite-integral (multiplicand expr) var)))
    ((and (exponentiation? expr) (eq? (base expr) var) (number? (power expr)))
     (make-product (/ 1 (+ (power expr) 1))
                   (make-exponentiation var (+ (power expr) 1))))
    (else
     (let ((result-of-attempt (try-simple-special-case expr var)))
        (if (eq? result-of-attempt #f) (error "Unknown expression type: INDEFINITE-INTEGRAL" expr)
            result-of-attempt)))))
(define (try-simple-special-case expr var)
  (let ((function (car expr)))
    (cond
      ((eq? function 'sin) (make-product -1 (make-special-function 'cos var)))
      ((eq? function 'cos) (make-special-function 'sin var))
      (else #f))))

(define (exact-definite-integral expr var a b)
  (let ((antiderivative (indefinite-integral expr var)))
    (- (evaluate-expr antiderivative var b)
       (evaluate-expr antiderivative var a))))
#|
(indefinite-integral '(5 * (x ^ 3)) 'x)
(indefinite-integral '(2 * x) 'x)
(exact-definite-integral '(x ^ 2) 'x 0 1)
|#
(indefinite-integral '((sin x) + 1) 'x)