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

; Dodgy function 
; (define member? (lambda (a lat)
;                   (cond ((not (atom? a)) #f)
;                         ((not (lat? lat)) #f)
;                         ((eq? a (car lat)) #t)
;                         (else (member? a (cdr lat))))))
; 
; (define l '("cat" "bog"))
; (member? "dog" l)

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
           ;(cons (cons (car lat) new) (multiinsertR new old (cdr lat)))) ; Given two atoms, cons will make a pair.
           (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat))
           (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) old)
           (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

; (define aList '("a" "b" "a" "a" "e" "f"))
; (multiinsertR "x" "a" aList)
; (multiinsertL "x" "a" aList)
; (multisubst "x" "a" aList)

(define all-nums
  (lambda (lat)
    (cond ((null? lat) (quote ()))
          ((number? (car lat)) 
           (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(define 2list '(1 "a" 2 "b" 3 "c"))
(all-nums 2list)

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eq? a (car lat)) (+ 1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

; (define food '("hotdog" "meat" "sausage" "hotdog" "hotdog" "dimsim"))
; (occur "hotdog" food)

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond ((null? lat) (quote ()))
          ((one? n) (cdr lat))
          (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))

(define asd '("lemon" "meringue" "salty" "pie"))
(rempick 3 asd)
