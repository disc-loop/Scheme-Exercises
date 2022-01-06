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

; New member? function written using equal?
(define member? 
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (equal? (car lat) a) (member? a (cdr lat)))))))

(define s1 '("dog" "cat" "cow" 2 "bird"))
(define s2 '("ham" "dog" 2 "cheese" 1 "burger" 10))

; Union returns a set (no-duplicates lat) which is constructed using the elements of two sets
; One way to do this is to append the first set to the second set, and remove any duplicates
; Check each element if they are members of the other set, if true, skip elem
; Because we are building a lat, we will return a null set. Our terminating case will be when the first set is null.
; cons onto the nat recursion of union
(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) 
           (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

;(union s1 s2)

; intersectAll returns a set containing the elems that are present in a list of sets
; Questions:
; is the set null? return empty set
; is first elem of first set in next set? cons elem onto nat recur 
; else? nat recur with list where the first elem of the first set is taken out 
(define intersectAll
  (lambda (lset)
    (cond ((null? (car lset)) (quote ()))
          ((member? (car (car lset)) (car (cdr lset)))
           (cons (car (car lset)) (intersectAll (cons (cdr (car lset)) (cdr lset)))))
          (else (intersectAll (cons (cdr (car lset)) (cdr lset)))))))

(define s3 (cons '("dog") (cons s1 (cons s2 (quote ())))))
;(intersectAll s3)

; Lessons from intersectAll:
; C8: Use help functions to abstract from representations - It got a bit confusing with all the cons, car and cdr
; Do not repeat yourself - There are two parts with that could have been abstracted (intersectAll (cons (cdr (car lset)) (cdr lset)))

; the pair? function takes a list and checks to see if it is a "pair" (not the Lisp kind). Returns bool
; 

(define a-pair?
  (lambda (l)
    (cond ((atom? l)#f)
          ((null? l)#f)
          ((null? (cdr l))#f)
          (else (null? (cdr (cdr l)))))))

(define lp '("dog" "cat")) 
;(a-pair? lp)

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define seconds
  (lambda (s)
    (cond ((null? s) '())
          (else (cons (second (car s)) (seconds (cdr s)))))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;(build lp)

(define set?
  (lambda (s)
    (cond ((null? s)#t)
          ((member? (car s) (cdr s))#f)
          (else (set? (cdr s)))))) 

(define firsts
  (lambda (l)
    (cond ((null? l) (quote ()))
          (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define set1 '(("cat" "dog") ("dog" "cow") ("pig" "fly")))
(define set2 '(("dog" "dog") ("bird" "cow") ("cat" "dog") ("pig" "fly")))
;(fun? set2)

; revrel swaps the first element with the second element in each pair in a set
; (define revrel
;   (lambda (s)
;     (cond ((null? s) (quote ()))
;           (else (cons (build (second (car s)) (first (car s)))
;                       (revrel (cdr s)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (s)
    (cond ((null? s) '())
          (else (cons (revpair (car s)) (revrel (cdr s)))))))

;(revrel set1)

; injective: one-to-one: each elem in codomain is mapped to by at most one elem in domain
; surjective: onto: each elem in codomain is mapped to by at least one elem in domain
; bijective: both: each elem in codomain is mapped to by one elem in domain

(define fullfun
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one
  (lambda (fun)
    (fun? (revrel fun))))

(fullfun set1)
(fullfun set2)
