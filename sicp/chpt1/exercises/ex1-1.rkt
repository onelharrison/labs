#lang sicp

; Exercise 1.1
;
; Below is a sequence of expressions. What is the result printed
; by the interpreter in response to each expression? Assume that
; the sequence is to be evaluated in the order in which it is
; presented.

; Ans: 10
10

; Ans: 12
(+ 5 3 4)

; Ans: 8
(- 9 1)

; Ans: 3
(/ 6 2)

; Ans: 6
(+ (* 2 4) (- 4 6))

; Ans: no output
(define a 3)

; Ans: no output
(define b (+ a 1))

; Ans: 19
(+ a b (* a b))

; Ans: #f
(= a b)

; Ans: 4
(if (and (> b a) (< b (* a b)))
  b
  a)

; Ans: 16
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

; Ans: 6
(+ 2 (if (> b a) b a))

; Ans: 16
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))
