#lang racket
; Ackermann's function.
; Grows very quickly, quicker than most functions
; It is still effectively computable
(define (Ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2) ; This is what makes the function return a multiple of 2, as it will keep going until the y in the call below becomes 1)
        (else (Ackermann (- x 1)
                         (Ackermann x (- y 1))))))
; Provided each time A is called, the function will call 2 more of A, until x = 0 or y = 1 (if y = 0, the function will exit before calling any others, i.e. y = 0 will never happen unless you pass the first call a zero).
; In the second A (in the else), x stays constant till y becomes 1, which then returns 2.
; 
(Ackermann 1 10)
(Ackermann 2 4) ; This and the function below produce the same output, probably because they reach the bounds of allocated memory
(Ackermann 3 3)

; 1. f(n) = 2n
; 2. g(n) = 2^n 
; Not sure how to figure this one out
; Looked up the answer, stared at the function, figured out how it behaved, figured it out.
; 3. h(n) = 2^n^n? ;
; Close, but it's 2^h(n-1), or 
; h(n) = 2^2^n ; is this true? verify it!
; Here is a great blog post that explains it:
; https://dev.to/tttaaannnggg/sicp-1-2-ackermann-s-function-and-contemplating-infinity-4o5c
; Spend some time studying the method employed here to understand complex functions
