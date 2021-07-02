#lang sicp

; Exercise 1.5
;
; Ben Bitdiddle has invented a test to determine whether the interpreter
; he is faced with is using applicativeorder evaluation or normal-order
; evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order
; evaluation?
;
; Ans: Ben will observe a non-terminating behavior with an interpreter that uses
; applicative-order evaluation.
;
; What behavior will he observe with an interpreter that uses normal-order
; evaluation? Explain your answer. (Assume that the evaluation rule for the special form
; if is the same whether the interpreter is using normal or applicative order: The
; predicate expression is evaluated first, and the result determines whether to evaluate
; the consequent or the alternative expression.)
;
; Ans: However, he will observe that the program returns 0 with an interpreter that uses
; normal-order evaluation, as show by the following expansion and reduction steps.
;
; (test 0 (p))
;
; 1. Expand the test procedure
; (if (= x 0) 0 y)
;
; 2. Replace its formal parameters with its arguments
; (if (= 0 0) 0 (p))
;
; 2. Reduce the if condition
; (if #t 0 (p))
;
; 3. Base on the if condition's result evaluate the consequent or aternative expression.
; In this case, the consequent expression is evaluated.
; 0
