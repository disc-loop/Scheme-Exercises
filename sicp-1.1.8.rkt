#lang racket

; Square Roots by Newton's Method

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) ; As new-if is a function, the interpreter will eval all the args first before applying, this means that new-if will never terminate, as it constantly has to eval sqrt-iter in the else clause.
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (new-if predicate then-clause else-clause) ; This will not behave as an if-cond, as any recursively defined input will cause the function to loop forever (regardless of whether it's if or cond; that was a red-herring).
  (cond (predicate then-clause)
        (else else-clause)))

(sqrt-iter 1.0 3)
