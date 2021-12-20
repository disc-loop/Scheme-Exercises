#lang racket

(define (sum-of-two-larger a b c)
          (+ (if (> a b)
                 a
                 b)
             (if (> a c)
                 a
                 c)))

(sum-of-two-larger 3 6 5)
