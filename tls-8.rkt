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

;(define rember-f
;  (lambda (test? a l)
;    (cond ((null? l) '())
;          ((test? (car l) a) (cdr l))
;          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define s1 '("dog" "cat" "tuna" "cow" 2 "bird" "tuna"))
(define s2 '("ham" "dog" 2 "cheese" 1 "burger" 10))
(define s3 (cons '("dog") (cons s1 (cons s2 (quote ())))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? (car l) a) (cdr l))
            (else (cons (car l) ((rember-f test?) a (cdr l))))))))

;((rember-f eq?) "dog" s1)
;((rember-f eqan?) 10 s2)
;((rember-f equal?) '("dog") s3)

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons new (cons old (cdr l))))
            (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old) (cons old (cons new (cdr l))))
            (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? old (car l)) (seq new old (cdr l)))
            (else (cons (car l) 
                        (((insert-g seq) new old (cdr l)))))))))

(define insertL 
  (insert-g 
    (lambda (new old l) 
      (cons new (cons old l)))))

(define insertR (insert-g seqR))

;((insert-g seqL) "bug" "dog" s1)
;((insert-g seqR) "bug" "dog" s1)

(define seqS 
  (lambda (old new l)
    (cons new l)))
(define subst
  (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+)+)
          ((eq? x '*)*)
          (else expt))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
        ((test? (car lat) a) 
         (multirember-f a (cdr lat)))
        (else (cons (car lat) 
                    (multirember-f a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-tuna (eq?-c "tuna"))

(define multiremberT
  (lambda (test? lat)
      (cond ((null? lat) '())
            ((test? (car lat)) (multiremberT test? (cdr lat)))
            (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define s4 '("dog" "cat" "tuna" "cow" 2 "bird" "tuna"))
;(multiremberT eq?-tuna s4)

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col '() '())) ; This is the stopper. We only apply col here at the end of the list
          ((eq? (car lat) a) 
           (multirember&co a (cdr lat) 
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car lat) seen))))) ; Otherwise, the matching atom is added to the seen list
          (else
            (multirember&co a (cdr lat)
                            (lambda (newlat seen)
                              (col (cons (car lat) newlat) seen))))))) ; Or, if it doesn't match, it gets added to newlat
; When the function gets to the stopper, it collects the two empty lists, and starts building the lists until it gets to the original col, whereby it is applied to the two lists. In this case, a-friend ignores the first argument and checks to see if the second argument is the null list and will return true if it is. 

(define a-friend
  (lambda (x y)
    (null? y)))

;(multirember&co "tuna" s4 a-friend)

; Inserts new to the left of oldL and to the right of oldR in lat if oldL and oldR are different (they must be different otherwise oldR will never be used as we match on the same value as oldL
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? oldL (car lat))
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? oldR (car lat))
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

;(multiinsertLR "sleepy" "tuna" "dog" s4)

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL)
           (multiinsertLR&co new oldL oldR (cdr lat) 
                             (lambda (newlat L R)
                               (col (cons new (cons oldL newlat)) (+ 1 L) R))))
          ((eq? (car lat) oldR)
           (multiinsertLR&co new oldL oldR (cdr lat) 
                             (lambda (newlat L R)
                               (col (cons new (cons oldR newlat)) L (+ 1 R)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat)
                                  (lambda (newlat L R)
                                    (col (cons (car lat) newlat) L R)))))))

;(define even?
;  (lambda (n)
;    (= (* (/ n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) 
           (cond ((even? (car l)) 
                  (cons (car l) (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define nums '(3 23 (2 4 5 (90 21) 24) 0))
(evens-only* nums)
