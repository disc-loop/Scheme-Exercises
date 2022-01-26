#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- 1 n)))

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

((lambda (nothing)
   (cond (nothing (quote something))
         (else (quote nothing))))#t)

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action))
          (else (list-to-action e)))))

(define atom-to-action
  (lambda (a)
    (cond ((number? a) *const)
          ((eq? a #t) *const)
          ((eq? a #f) *const)
          ((eq? a 'cons) *const)
          ((eq? a 'car) *const)
          ((eq? a 'cdr) *const)
          ((eq? a 'null?) *const)
          ((eq? a 'eq?) *const)
          ((eq? a 'atom?) *const)
          ((eq? a 'zero?) *const)
          ((eq? a 'add1) *const)
          ((eq? a 'sub1) *const)
          ((eq? a 'number?) *const)
          (else *identifier))))

(define list-to-action
  (lambda (l)
    (cond ((atom? (car l))
          (cond ((eq? (car l) 'quote) *quote) 
                ((eq? (car l) 'lambda) *lambda) 
                ((eq? (car l) 'cond) *cond) 
                (else *application)))
          (else *application))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (second e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

; These are for determining the value of a function. Table are the args passed in, the formals are the params, and the body is the body of the function
(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table))))) 

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x 'else))
          (else #f))))
(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(define evlis 
  (lambda (args table)
    (cond ((null? args) '())
          (else (cons (meaning (car args) table)
                      (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

(define function-of car) 
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond ((primitive? fun)
           (apply-primitive name vals))
          ((non-primitive? fun)
           (apply-closure (second fun) vals)))))

(define :atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) 'primitive) #t)
          ((eq? (car x) 'non-primitive) #f)
          (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name 'cons)
           (cons (first vals) (second vals)))
          ((eq? name 'car)
           (car (first vals)))
          ((eq? name 'cdr)
           (cdr (first vals)))
          ((eq? name 'null?)
           (null? (first vals)))
          ((eq? name 'eq?)
           (eq? (first vals) (second vals)))
          ((eq? name 'atom?)
           (:atom? (first vals)))
          ((eq? name 'zero?)
           (zero? (first vals)))
          ((eq? name 'add1)
           (add1 (first vals)))
          ((eq? name 'sub1)
           (sub1 (first vals)))
          ((eq? name 'number?)
           (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure) 
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))
