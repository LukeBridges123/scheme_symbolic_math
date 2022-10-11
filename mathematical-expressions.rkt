#lang racket
#|
"mathematical-expressions" is a program for building and evaluating mathematical expressions. A "mathematical expression" is defined recursively as either
a number, a variable (i.e. one of Scheme's quoted "symbols"), a list of the form (op expr) where expr is a mathematical expression and op is a unary operation,
or a list of the form (expr1 op expr2) where expr1 and expr2 are expressions and op is a binary operator. The currently-supported binary operations are +, -, *, /,
and ^, while the currently-supported unary operations are sin, cos, tan, and ln.

The first part of the program consists of constructors and selectors for mathematical expressions. These generally contain rules for automatically simplifying expressions
so that, for instance, (make-sum 'x 0) simply evaluates to x. The second part consists of a function evaluate-expr that evaluates expressions, given an assignment of
numerical values to the variables within an expression, and another function expr->function which takes in expressions and creates equivalent executable functions.
|#

;(provide (all-defined-out)) means that all the definitions internal to mathematical-expressions will be available in programs that (require) it,
;in particular symbolic-differentiation.
(provide (all-defined-out))

(define variable? symbol?)
(define (same-variable? var1 var2)
  (and (variable? var1) (variable? var2) (eq? var1 var2)))
(define (=number? expr num)
  (and (number? expr) (= expr num)))
#|
"sum" = list of the form (expr1 + expr2), where expr1, expr2 are expressions. "addend" is the first expression, "augend" is the last.
Currently-implemented simplification rules: x + 0 = x; 0 + x = x; x + x = 0; x + y = (+ x y) when x and y are numbers;
x + (-y) = x - y (currently implemented for numbers only); x + x = 2x
|#
(define (make-sum expr1 expr2)
  (cond
    ((=number? expr1 0) expr2)
    ((=number? expr2 0) expr1)
    ((and (number? expr1) (number? expr2)) (+ expr1 expr2))
    ((number? expr2) (< 0 expr2) (make-difference expr1 (abs expr2)))
    ((equal? expr1 expr2) (make-product 2 expr1))
    (else (list expr1 '+ expr2))))

(define (addend sum)
  (car sum))
(define (augend sum)
  (caddr sum))
(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))

#|
"difference" = list of the form (expr1 - expr2), where expr1, expr2 are expressions. "minuend" is the first expression, "subtrahend" is the last.
Currently-implemented simplification rules: x - 0 = x; 0 - x = -x; x - x = 0; x - y = (- x y) when x and y are numbers.
|# 
(define (make-difference expr1 expr2)
  (cond
    ((=number? expr2 0) expr1)
    ((=number? expr1 0) (make-product -1 expr2))
    ((equal? expr1 expr2) 0)
    ((and (number? expr1) (number? expr2)) (- expr1 expr2))
    (else (list expr1 '- expr2))))
(define (minuend diff)
  (car diff))
(define (subtrahend diff)
  (caddr diff))
(define (difference? expr)
  (and (pair? expr) (eq? (cadr expr) '-)))

#|
"product" = list of the form (expr1 * expr2), where expr1, expr2 are expressions. "multiplier" is the first expression, "multiplicand" is the last.
Currently-implemented simplification rules: x * 0 = 0 * x = 0; 1 * x = x * 1 = x; x * x = x^2; x * y = (* x y) when x and y are numbers; (x^a) * (x^b) = x^(a+b)
When one of the expressions is a number, makes sure that the number goes first in the product, so that it will always produce expressions like (2 * x) rather than
(x * 2). This makes it easier for things like the integral function to "pull out" constant factors.
It also makes it easier to implement rules that simplify expressions like (a * (b * x)) and ((a * x) * b), where a and b are numbers, into ((a * b) * x).
The make-product function can "assume" that, if one of the expressions passed to it is a product, that product will have its numerical factors come first.
This allows for reducing the number of checks needed in order to determine when the ((a * b) * x) simplification rule should be applied. 
|#
(define (make-product expr1 expr2)
  (cond
    ((or (=number? expr1 0) (=number? expr2 0)) 0)
    ((=number? expr1 1) expr2)
    ((=number? expr2 1) expr1)
    ((and (number? expr1) (number? expr2)) (* expr1 expr2))
    ((and (number-coeff? expr1) (number-coeff? expr2)) (make-product (* (multiplier expr1) (multiplier expr2))
                                                                     (make-product (multiplicand expr1)
                                                                                   (multiplicand expr2))))
    ((and (number? expr1) (number-coeff? expr2)) (make-product (* expr1 (multiplier expr2))
                                                               (multiplicand expr2)))
    ((and (number? expr2) (number-coeff? expr1)) (make-product (* expr2 (multiplier expr1))
                                                               (multiplicand expr1)))
                       
    ((number? expr2) (list expr2 '* expr1))
    ((equal? expr1 expr2) (make-exponentiation expr1 2))
    ((and (exponentiation? expr1) (exponentiation? expr2) (equal? (base expr1) (base expr2)))
     (make-exponentiation (base expr1)
                          (make-sum (power expr1) (power expr2))))
    (else (list expr1 '* expr2))))
(define (multiplier prod)
  (car prod))
(define (multiplicand prod)
  (caddr prod))
(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))
#|number-coeff checks whether an expression is a product whose first term is a number. (If the product has been made with the
make-product constructor, which always puts numbers as the first term in a product, then the case where the second term is a number
need not be checked.|#
(define (number-coeff? expr)
  (and (product? expr) (number? (multiplier expr))))
#|
"quotient" = list of the form (expr1 / expr2) where expr1 and expr2 are expressions. "numer" and "demon" are the first and second expressions, respectively.
Currently-implemented simplification rules: 0 / x = 0; x / 1 = x; x / y = (/ x y) when x, y are numbers; x / x = 1; (x^a)/(x^b) = x^(a-b)
|#
(define (make-quotient expr1 expr2)
  (cond
    ((=number? expr2 0) (error "division by 0: MAKE-QUOTIENT" expr1 expr2))
    ((=number? expr1 0) 0)
    ((=number? expr2 1) expr1)
    ((and (number? expr1) (number? expr2)) (/ expr1 expr2))
    ((eq? expr1 expr2) 1)
    ((and (exponentiation? expr1) (exponentiation? expr2) (equal? (base expr1) (base expr2)))
     (make-exponentiation (base expr1)
                          (make-difference (power expr1) (power expr2))))
    (else (list expr1 '/ expr2))))
(define (numer quotient)
  (car quotient))
(define (denom quotient)
  (caddr quotient))
(define (quotient? expr)
  (eq? (cadr expr) '/))
#|
"special function": list of the form (f expr), where f is one of the currently-implemented special functions:
sin, cos, tan, ln
and expr is just an expression. No simplification rules have been implemented so far.
|#
(define (make-special-function function part-in-parentheses)
  (list function part-in-parentheses))
(define (special-function? expr)
  (let ((f (car expr)))
    (or (eq? f 'sin)
        (eq? f 'cos)
        (eq? f 'tan)
        (eq? f 'ln)
        (eq? f 'arcsin)
        (eq? f 'arccos)
        (eq? f 'arctan))))

(define (make-exponentiation expr1 expr2)
  (cond
    ((=number? expr1 1) 1)
    ((=number? expr2 0) 1)
    ((=number? expr2 1) expr1)
    ((and (number? expr1) (number? expr2)) (expt expr1 expr2))
    ((exponentiation? expr1) (make-exponentiation (base expr1)
                                                  (make-product (power expr1) expr2)))
    (else (list expr1 '^ expr2))))
(define (base exponentiation)
  (car exponentiation))
(define (power exponentiation)
  (caddr exponentiation))
(define (exponentiation? expr)
  (and (pair? expr) (eq? (cadr expr) '^)))

(define (expr-with-binary-operation? expr)
  (and (pair? expr) (= 3 (length expr))))
(define (expr-with-unary-operation? expr)
  (and (pair? expr) (= 2 (length expr))))
(define (symbol-to-function symbol)
  (cond
    ((eq? symbol '+) +)
    ((eq? symbol '-) -)
    ((eq? symbol '*) *)
    ((eq? symbol '/) /)
    ((eq? symbol '^) expt)
    ((eq? symbol 'sin) sin)
    ((eq? symbol 'cos) cos)
    ((eq? symbol 'tan) tan)
    ((eq? symbol 'ln) log)
    ((eq? symbol 'arcsin) asin)
    ((eq? symbol 'arccos) acos)
    ((eq? symbol 'arctan) atan) 
    (else (error "unknown operation: SYMBOL-TO-FUNCTION" symbol))))


(define (evaluate-expr expr var value-of-var)
  (cond
    ((number? expr) expr)
    ((eq? expr 'e) (exp 1))
    ((eq? expr var) value-of-var)
    ((expr-with-binary-operation? expr) ((symbol-to-function (cadr expr))
                                         (evaluate-expr (car expr) var value-of-var)
                                         (evaluate-expr (caddr expr) var value-of-var)))
    ((expr-with-unary-operation? expr) ((symbol-to-function (car expr))
                                        (evaluate-expr (cadr expr) var value-of-var)))
    (else (error "unknown expression type: EVALUATE-EXPR" expr))))
(define (expr->function expr var)
  (lambda (x)
    (evaluate-expr expr var x)))

