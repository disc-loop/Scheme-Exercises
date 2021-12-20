#lang racket

; 1.11
; Define a function that works both recursively and iteratively based on the one in the book
; Looks like the strategy is to first find the basic transformation for your variables (one for each iterative call)
; Next, have a counter that tracks how many iterations need to be completed
(define (f n)
  (fr n))

; Recursive version of function
(define (fr n)
  (if (< n 3)
    n
    (+ ; Not tail recursive as it has this deferred operation
      (fr (- n 1))
      (* (fr (- n 2)) 2) 
      (* (fr (- n 3)) 3))))

; The strategy is to try and conceive of the recursive section as a loop, where there is a breakout case and accumulators for each recursive call
; Here, n acts as as counter.
; Accordingly, we need 3 accumulators for each call. Let's have them as a b and c
; Another way of thinking about it is as a recurrence relation. Determine the values for a few of the base cases and see how the iterations are defined in terms of the previous ones
; Start with very basic cases
; (f 0) = 0 
; (f 1) = 1 
; (f 2) = 2
; Notice how you can think of (*(f(- n 2))2) as (*(f 1)2). (i.e. its the previous iteration's result times 2)
; We got this information from the function definition we were provided
; (f 3) = (f 2) + (*(f 1)2) + (*(f 0)3) = 2 + (* 2 2) + (* 0 3) = 4
; (f 4) = (f 3) + (*(f 2)2) + (*(f 1)3) = 6 + 4 + 3 = 11
; Now observe the pattern: in the next iteration, the first term becomes the result of the previous function. 
; The second term is the product of the iteration before that and 2, and the third is the product of the iteration before that and 3
; 
; (define (f2 n)
;   (fi n 2 1 0))
; 
; ; Iterative version of function
; ; Will count down, but will run through the algo n amount of times, accumulating the sum as it runs
; (define (fi i a b c) 
;   (cond ((= i 2) i)
;         ((= i 1) i)
;         ((= i 0) i)
;         (else s)))
; (c b)
; (b a)
; (a (a + (* b 2) 
; 
; f(n) = (sum f(n-1) 2f(n-2) 3f(n-3))
; f(n) = n, n < 3
; (f 0) = 0 
; (f 1) = 1 
; (f 2) = 2
; (f 3) = (f 2) + (* 1 2) + (* 0 3) = 6
; (f 4) = (f 6) + (* 2 2) + (* 1 3) = 13
; (else (fi (- i 1) (+ a (* b 2) (* c 3))))))
(define (f2 n)
  (fi n 2 1 0))

(define (fi i f1 f2 f3)
  (cond ((< i 2) i)
        ((< i 3) f1)
        (else (fi (- i 1) (+ f1 (* 2 f2) (* 3 f3)) f1 f2))))
; Here, i is decremented by 1, the current value of a (i.e. f(n-1)) is 

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f2 0)
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)

;(f 3)
;(f 3) -> (+ (f 2) (* (f 1) 2) (* (f 0) 3)) -> (+ 2 2 0) -> 4
;(f 4) -> (+ (f 3) (* (f 2) 2) (* (f 1) 3)) -> (+ (f 3) 4 3) -> (+ ((+ (f 2) (* (f 1) 2) (* (f 0) 3)) -> (+ 2 2 0) -> 4) -> 11


