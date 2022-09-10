#lang scheme
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


(define (make-sum expr1 expr2)
  (cond
    ((=number? expr1 0) expr2)
    ((=number? expr2 0) expr1)
    ((and (number? expr1) (number? expr2)) (+ expr1 expr2))
    (else (list expr1 '+ expr2))))

(define (addend sum)
  (car sum))
(define (augend sum)
  (caddr sum))
(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))

#|
A "difference" is an expression represented as a list of the form (expr1 - expr2) where expr1, expr2 are mathematical expressions. "minuend" is the first expression
and "subtrahend" is the last. Currently, make-difference can automatically simplify expressions where at least one of the following is true:
the subtrahend is 0; the minuend is 0; the subtrahend and minuend are equal; the subtrahend and minuend are both numbers. 
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

(define (make-product expr1 expr2)
  (cond
    ((or (=number? expr1 0) (=number? expr2 0)) 0)
    ((=number? expr1 1) expr2)
    ((=number? expr2 1) expr1)
    ((and (exponentiation? expr1) (exponentiation? expr2) (equal? (base expr1) (base expr2)))
     (make-exponentiation (base expr1)
                          (make-sum (power expr1) (power expr2))))
    ((and (number? expr1) (number? expr2)) (* expr1 expr2))
    (else (list expr1 '* expr2))))
(define (multiplier prod)
  (car prod))
(define (multiplicand prod)
  (caddr prod))
(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))

(define (make-quotient numer denom)
  (cond
    ((=number? numer 0) 0)
    ((=number? denom 1) numer)
    ((=number? denom 0) (error "division by 0: MAKE-QUOTIENT" numer denom))
    ((and (number? numer) (number? denom)) (/ numer denom))
    ((eq? numer denom) 1)
    (else (list numer '/ denom))))
(define (numer quotient)
  (car quotient))
(define (denom quotient)
  (caddr quotient))
(define (quotient? expr)
  (eq? (cadr expr) '/))

(define (make-special-function function part-in-parentheses)
  (list function part-in-parentheses))
(define (special-function? expr)
  (let ((f (car expr)))
    (or (eq? f 'sin)
        (eq? f 'cos)
        (eq? f 'tan)
        (eq? f 'ln))))

(define (make-exponentiation expr1 expr2)
  (cond
    ((=number? expr1 1) 1)
    ((=number? expr2 0) 1)
    ((=number? expr2 1) expr1)
    ((and (number? expr1) (number? expr2)) (expt expr1 expr2))
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
    ((eq? symbol 'ln) log)))


(define (evaluate-expr expr var value-of-var)
  (cond
    ((number? expr) expr)
    ((eq? expr 'e) (exp 1))
    ((eq? expr var) value-of-var)
    ((expr-with-binary-operation? expr) ((symbol-to-function (cadr expr))
                                         (evaluate-expr (car expr) var value-of-var)
                                         (evaluate-expr (caddr expr) var value-of-var)))
    ((expr-with-unary-operation? expr) ((symbol-to-function (car expr))
                                        (evaluate-expr (cadr expr) var value-of-var)))))
(define (expr->function expr var)
  (lambda (x)
    (evaluate-expr expr var x)))