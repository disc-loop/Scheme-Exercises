#lang racket

(define s4 '("dog" "cat" "tuna" "cow" 2 "bird" "tuna"))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-tuna (eq?-c "tuna"))

(define multiremberT 
  (lambda (test? lat)
    (cond ((null lat) '())
          (else (test? lat)))))

(multiremberT eq?-tuna "tuna")

;(define multiremberT
;  (lambda (test? lat)
;      (cond ((null? lat) '())
;            ((test? (car lat)) (multiremberT test? (cdr lat)))
;            (else (cons (car lat) (multiremberT test? (cdr lat)))))))

;(multiremberT eq?-tuna s4)
