#lang racket

(define (pas n)
  (define (p r c)
    (if (or (<= c 1) (= c r))
      1
      (s (- r 1) (+ c 1)

; Unused
; Recursively computes the power of exponent e of base b
(define (pow b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (* b (pow b (- e 1))))))

(test 2)
(test 3)
(test 4)
(test 5)
