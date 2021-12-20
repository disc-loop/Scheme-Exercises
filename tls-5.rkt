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

(define rember*
  (lambda (a l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) ; If elem is atom
            (cond ((eq? a (car l)) (rember* a (cdr l))) ; If match, recurse with the rest of the list
                  (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l)) ; If elem is not atom, recursively traverse down the sublists until an atom is found
                      (rember* a (cdr l))))))) ; Add the recursion of rember which traverses the first element of the list to the recursion of rember which traverses the rest of the list

(define food '((("tomato" "sauce"))
           (("bean") "sauce")
           ("and" (("flying")) "sauce"))) 
; no atom for (((tomato sauce)) (((bean) sauce) (and ((flying)) sauce))): (cons ((tomato sauce)) (((bean) sauce) (and ((flying)) sauce)))
  ; no atom for (tomato sauce): (cons (tomato sauce) ((tomato sauce)))
    ; atom for tomato: not eq to sauce: pass in next 
      ; atom: eq: recurse with rest
        ; null: return the empty list
      ; etc.

; Explanation:
; The function removes a member in a list and any sublists. It looks for atoms in the first element, and will recursively traverse the sublists until it finds the member. When it finds the member, it continues through the sublist until it reaches the end of the sublist, upon where it will return the null list. For each member which is not the member to remove, the function adds the member to the result of recursing with the rest of the sublist.

(define insertR*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l)) 
            (cond ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
                  (else (cons (car l) (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))))) ; The first recursion goes down the sublist of car l, the second one moves the function across to the next element. We need to cons the first one onto the second as this rebuilds each element in the array.
; Roughly, for al, this looks like this:
; else -> car -> else -> car -> else -> car -> atom
;      -> cdr -> else -> car -> atom 
;                     -> cdr -> else -> etc

;(insertR* "boy" "sauce" al)

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l)) 
           (cond ((eq? (car l) a) (+ 1 (occur* a (cdr l))))
                 (else (occur* a (cdr l)))))
          (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define animals '("dog" ("cat" ( "dog" "monkey" ) "dog" "cat")))
;(occur* "dog" animals) 

(define subst*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? old (car l)) 
                  (cons new (subst* new old (cdr l))))
                 (else (cons (car l) (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;(subst* "cow" "dog" animals)

(define insertL*
  (lambda (new old l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons new (cons old (insertL* new old (cdr l)))))
                 (else (cons (car l) (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;(insertL* "cow" "dog" animals)

(define member* 
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (cond ((eq? (car l) a) #t)
                 (else (member* a (cdr l)))))
          (else (or (member* a (car l)) (member* a (cdr l)))))))

;(member* "monkey" animals)

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))
;(leftmost animals)

; (atom? (eq? a b) 

; Write down the cases
; For two lists to be the same:
; Each atom must be eq to the same elem for both
; Null list counts
; So:
; Terminate: return bool, check on null for both
; Recurse: using car and cdr for each list. Because we are checking that all elems are the same, we must recurse with one of the conditional operators, i.e. and.
; Cases:
; Both are null -> t
; One is null -> f
; Neither are null -> check if...
; If both are atoms -> check if...
; eq -> recur with and, using cdr
; not eq -> f
; If one is atom -> f
; If neither are atoms -> recur with and, using car and cdr

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2))#t)
          ((or (null? l1) (null? l2))#f) ; We already checked AND above
          ((and (atom? (car l1)) (atom? (car l2))) 
           (and (eq? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2))))
          ((or (atom? (car l1)) (atom? (car l2)))#f)
          (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;(eqlist? animals animals)
;(eqlist? animals food)

(define eqal?
  (lambda (a b)
    (cond ((and (atom? a) (atom? b)) (eq? a b))
          ((or (atom? a) (atom? b)) #f)
          (else (eqlist? a b)))))

;(eqal? animals food)
;(eqal? animals animals)
;(eqal? 2 2)
;(eqal? "he" "ha")

          
