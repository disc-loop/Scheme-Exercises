#lang racket

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (plus a b) ; This generates a recursive process as the inc operation is deferred
  (if (= a 0)
      b
      (inc (plus (dec a) b))))

(plus 2 5)
(plus 5 10)

; (define (plus a b) ; This generates an iterative process as there are no deferred operations
;   (if (= a 0)
;       b
;       (plus (dec a) (inc b))))
