#lang racket
(require "mathematical-expressions.rkt")
(provide (all-defined-out))

(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((variable? expr) (if (same-variable? expr var) 1
                          0))
    ;(deriv (f + g)) = (deriv f) + (deriv g)
    ((sum? expr) (make-sum (deriv (addend expr) var) (deriv (augend expr) var)))
    ((product? expr) (apply-product-rule (multiplier expr) (multiplicand expr) var))
    ((quotient? expr) (apply-quotient-rule (numer expr) (denom expr) var))
    ((exponentiation? expr) (apply-exponentiation-rules expr var))
    ((special-function? expr) (apply-special-function-rules expr var))
    (else (error "unknown expression type: DERIV" expr))))

;(deriv (u * v)) = (f * (deriv g)) + ((deriv f) * g))
(define (apply-product-rule u v var)
  (make-sum
   (make-product (deriv u var) v)
   (make-product u (deriv v var))))
;(deriv (f / g)) = ((deriv f) * g) - (f * (deriv g)) / g^2
(define (apply-quotient-rule u v var)
  (make-quotient
   (make-difference (make-product (deriv u var) v)
                    (make-product u (deriv v var)))
   (make-exponentiation v 2)))
#|
Handles the rules for many different types of exponentiation. When the base is e, leave the expression as is except for applying the chain rule.
When the base is a number b, implicitly treat b^v as (e^(ln b))^v = e ^ (v * ln b). Applying the chain rule gets (e ^ (v * ln b)) * (ln b * (deriv v)) =
b^v * ln b * deriv v.
When the power is a number, apply the standard power rule and the chain rule, D(u^a) = a(u^(a-1)) * D(u).
In the case where neither the base nor an exponent is a number or e (e.g. for x^x), just throw an error. Support for fully general exponentiations may be brought in
later.
|#
(define (apply-exponentiation-rules expr var)
  (cond
    ((eq? (base expr) 'e) (make-product expr (deriv (power expr) var)))
    ((number? (base expr)) (make-product (make-product expr (make-special-function 'ln (base expr)))
                                         (deriv (power expr) var)));
    ((number? (power expr))  
     (make-product
      (make-product (power expr)
                    (make-exponentiation (base expr) (- (power expr) 1)))
      (deriv (base expr) var)))
    (else (error "unknown expression type: DERIV" expr))))
;One big switch statement of all the currently-available possibilities for what a special function could be. Chain rule is used throughout.
(define (apply-special-function-rules expr var)
  (let ((outer-function (car expr))
        (inner-function (cadr expr)))
    (case outer-function
      ('sin (make-product (make-special-function 'cos inner-function)
                          (deriv inner-function var)))
      ('cos (make-product (make-product -1 (make-special-function 'sin inner-function))
                          (deriv inner-function var)))
      ('tan (make-sum 1 (make-exponentiation (make-special-function 'tan var) 2)))
      ('ln (make-quotient (deriv inner-function var)
                          inner-function))
      ('arcsin (make-product (make-exponentiation (make-difference 1
                                                                   (make-exponentiation inner-function 2))
                                                  -1/2)
                             (deriv inner-function var)))
      ('arccos
       (make-product (make-product -1 (make-exponentiation (make-difference 1
                                                                            (make-exponentiation inner-function 2))
                                                           -1/2))
                     (deriv inner-function var)))
      ('arctan (make-product (make-quotient 1 (make-sum (make-exponentiation inner-function 2)
                                                        1))
                             (deriv inner-function var)))
                                                                 
      (else (error "unknown expression type: DERIV" expr)))))


(define (taylor-series f var center num-of-terms)
  (define (taylor-series-iter nth-deriv degree-of-curr-term partial-taylor-series n-factorial)
    (if (= num-of-terms degree-of-curr-term) partial-taylor-series
        (let* ((coeff (make-quotient (evaluate-expr nth-deriv var center)
                                     n-factorial))
               (curr-term (make-product coeff
                                        (make-exponentiation (make-difference var center)
                                                             degree-of-curr-term))))
          (taylor-series-iter (deriv nth-deriv var)
                              (+ degree-of-curr-term 1)
                              (make-sum partial-taylor-series curr-term)
                              (* n-factorial (+ degree-of-curr-term 1))))))
  (taylor-series-iter f 0 0 1))
#|
Finds the gradient of a function R^n -> R. It takes in an expression f (meant to be a function of n variables) and a list of variables (x1 x2 ... xn).
Returns a vector (i.e. list, not one of Scheme's "vector" arrays) where the ith element is the partial derivative of f with respect to xi.
|#
(define (gradient f vars)
  (map (lambda (var) (deriv f var)) vars))
#|
Finds the derivative of a function R -> R^n. The function should be represented as a vector of functions of the form f = (g1(t), g2(t), ... gn(t)), i.e.
all single-variable functions depending on the same variable.
|#
(define (deriv-parameterized-path f var)
  (map (lambda (g) (deriv g var)) f))
#|
Given a list of variables vars, of the form (x1, x2, .. xn), a function R^n -> R depending on all the vars

(define (tangent-hyperplane f vars point)
  (let ((partial-derivs (gradient f vars))
        (evaluated-partials (map evaluate-expr partial-derivs vars)))
    (make-sum |#
;(gradient (make-product 'x (make-product 'y 'z)) '(x y z))


#|
(evaluate-expr (taylor-series '(e ^ x) 'x 1 10) 'x 1.0)
(evaluate-expr (taylor-series '(e ^ x) 'x 0 10) 'x 2.0)
(taylor-series '(sin x) 'x 0 6)
(taylor-series '(cos x) 'x 0 6)
|#
    
     
#| tests
(evaluate-expr (make-sum 'x 'x) 'x 2)
(deriv '(x + x) 'x)
(deriv '(5 * x) 'x)
(evaluate-expr (deriv (make-exponentiation (make-sum 'x 3) 4) 'x) 'x -1)
(deriv '((x + 3) ^ 4) 'x)
(deriv '((sin x) / x) 'x)
(deriv '(ln x) 'x)
(define square (expr->function '(x ^ 2) 'x))
(square 4)
(square 0.5)
(deriv '(2 ^ x) 'x)
(make-product '(x ^ 2) '(x ^ 3))
|#

