#lang racket

(define numbered?
  (lambda (r)
    (cond ((rep?)
           ((number? (car r)) (numbered (cdr r))))
          (else #f))))
