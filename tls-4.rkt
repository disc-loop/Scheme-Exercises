#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add (add1 n) (sub1 m))))))

(define sub
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (add 
                  (car tup) 
                  (addtup (cdr tup)))))))

(define mlt
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (add 
                  n 
                  (mlt n (sub1 m)))))))

(mlt 5 3)
(mlt 2 0)
(mlt 12 3)
