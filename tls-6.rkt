#lang racket

; Checks if a given s-expression is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Checks if a given list is a list of atoms
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

; An arithmetic expression is either an atom (including numbers), or two arithmetic expressions combined by +, * or ^

; Check if the arithmetic expression contains only numbered atoms
(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; Before simplification
;          
;           (and (numbered? (car aexp) (numbered? (cdr (cdr aexp))))))
;          ((eq? (car (cdr aexp)) (quote *))
;           (and (numbered? (car aexp) (numbered? (cdr (cdr aexp))))))
;          ((eq? (car (cdr aexp)) (quote ↑))
;           (and (numbered? (car aexp)) (numbered? (cdr (cdr aexp))))))))

; Make sure to have spaces between the nums
(define axpr '(2 + (5 * (3 ^ 2))))
(numbered? axpr)

; value returns the natural value of a numbered arithmetic expression
; Questions first, structure later
; I.e. build up from cases

; (define value
;   (lambda (nexp)
;     (cond ((atom? nexp) )
;           ((eq? (car (cdr nexp)) (quote +)) )
;           ((eq? (car (cdr nexp)) (quote *)) )
;           (else ))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) )
          ((eq? (car (cdr nexp)) (quote +)) )
          ((eq? (car (cdr nexp)) (quote *)) )
          (else ))))
