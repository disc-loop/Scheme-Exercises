#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define pick
  (lambda (n lat)
    (cond ((zero? (- n 1)) (car lat))
          (else (pick (- n 1) (cdr lat))))))

(define a-pair?
  (lambda (l)
    (cond ((atom? l)#f)
          ((null? l)#f)
          ((null? (cdr l))#f)
          (else (null? (cdr (cdr l)))))))

; Given an atom, a symbol or number, and a lat, keep-looking will continue to look for an atom as long as the sorn is a number. For this reason, if the atom pick finds is a number, it may run forever. The point of this exercise is that you cannot tell.
(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

; Tries to find an atom in a lat. May not terminate. This is a partial function as it is not defined for all possible arguments of a given type. We usually interpret "not defined" as one of several things, including undefined behaviour, exceptions or non-termination.
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define l1 '(2 "dog" 2 4 9 "cat"))
;(looking 9 l1)

(define eternity
  (lambda (x)
    (eternity x)))

(define shift
  (lambda (l)
    (build (first (first l))
           (build (second (first l))
                  (second l)))))

(define align
  (lambda (pora)
    (cond ((atom? (first pora)) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (lat)
    (cond ((null? lat) 0)
          ((atom? lat) 1)
          (else (+ (length* (car lat)) (length* (cdr lat)))))))

;(length* l1)

(define weight*
  (lambda (pora)
    (cond ((null? pora) 0)
          ((atom? pora) 1)
          (else (+ (* 2 (weight* (first pora))) (weight* (second pora)))))))

;(weight* l1)

; On page 153, the book says that length is inadequate for counting the arguments for align. This is because we are interested in whether the arguments for align become smaller as we recur on its argument (i.e. closer to termination at the first cond line). Length is inadequate as it just counts the entire argument which produces a constant value as we recur on pora (it just shifts the values). What we want is something that allows us to compare the first against the second part of the argument, as thats what we look for in the termination condition. If we use a function that counts a pora, but gives extra weight to the number of atoms in the first element, we will be able to see that the value decreases as the number of atoms in the first element gets shifted over to the second element. We are trying to prove whether or not we will reach the termination condition, i.e. if align is partial or total (its total).

; An attempt to define will-stop.
; A given function must meet the following conditions to stop:
; A terminating condition which checks at least one of the arguments
; This argument must change, and tend towards the termination condition
; If the argument does not change for many cycles, but another argument does, which will eventually cause the original argument to change, then it must be considered to be part of the original argument (as in, an expression of which)
; e.g. we can see that the second condition progresses the function towards the termination condition
;(define func
;  (lambda (A B)
;    (cond ((= A 1) 'terminated)
;          ((< B 5) (func A (+ B 1)))
;          (else (func 1 B))))) 

