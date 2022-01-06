#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (= a1 a2))
          ((or (number? a1) (number? a2)) #f)
          (else (eq? a1 a2)))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2))#f)
          (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2))#t)
          ((or (null? l1) (null? l2))#f)
          (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; New member? function written using equal?
(define member? 
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

(define rember
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((equal? (car l) a) (cdr l))
          (else (cons (car l) (rember a (cdr l)))))))

(define rember-f
  (lambda (test? a l)
    (cond ((null? l) '())
          ((test? (car l) a) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define s1 '("dog" "cat" "cow" 2 "bird"))
(define s2 '("ham" "dog" 2 "cheese" 1 "burger" 10))
(define s3 (cons '("dog") (cons s1 (cons s2 (quote ())))))

; (rember-f equal? "dog" s1)
; (rember-f equal? 10 s2)
; (rember-f equal? '("dog") s3)

(define eq-?
  (lambda (x)
    (lambda (x)
      (eq? x x))))
