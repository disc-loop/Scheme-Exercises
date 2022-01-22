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

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

(define s1 '("dog" "cat" "cow"))
(define s2 '("bark" "meow" "moo"))

(define e1 (new-entry s1 s2))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) entry-f)
          ((eq? (car names) name) (car values))
          (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;(lookup-in-entry "horse" e1 "value not found")

(define table '(e1))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table absent-f)
    (cond ((null? table) absent-f)
          (else (lookup-in-entry name table 
                                 (lambda (name) 
                                   (lookup-in-table name (cdr table) absent-f)))))))

(value e)
