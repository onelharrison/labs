#lang sicp

; Exercise 1.3
;
; Define a procedure that takes three numbers as arguments
; and returns the sum of the squares of the two larger
; numbers.

(define (square n)
  (* n n))

(define (sum-of-squares n m)
  (+ (square n) (square m)))

; Same
; a > b > c
; b > a > c

; Same
; a > c > b
; c > a > b

; Same
; b > c > a
; c > b > a

(define (sum-of-squares-of-greater-two a b c)
  (cond
    ((and (< c a) (< c b)) (sum-of-squares a b))
    ((and (< b a) (< b c)) (sum-of-squares a c))
    (else (sum-of-squares b c))))

; Ans: 0
(sum-of-squares-of-greater-two 0 0 0)

; Ans: 5
(sum-of-squares-of-greater-two 0 1 2)

; Ans: 8
(sum-of-squares-of-greater-two 0 2 2)

; Ans: 8
(sum-of-squares-of-greater-two 2 2 0)

; Ans: 18
(sum-of-squares-of-greater-two 3 1 3)
