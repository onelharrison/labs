#lang sicp

; Exercise 1.4
;
; Observe that our model of evaluation allows for combinations
; whose operators are compound expressions. Use this observation
; to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 20)

; Substitution model for procedure evaluation (normal-order evaluation) 
;
; 1. Retrieve the body of the a-plus-abs-b procedure.
;
; ((if (> b 0) + - ) a b)
;
; 2. Replace the formal parameters a and b with their respective
; arguments.
;
; ((if (> 20 0) + -) 10 20)
;
; 3. Evaluate the > operator to get the greater-than procedure to apply to
; 20 and 0 in the if condition. The operands 20 and 0 are evaluated and their values
; passed as arguments to the greater-than procedure, resulting in the following
; reduction.
;
; ((if #t + -) 10 20)
;
; 4. #t is passed as an argument to the if operator. The if expression is reduced to the
; + procedure.
;
; (+ 10 20)
;
; 5. 10 and 20 are evaluated to their values and passed as arguments to the + procedure,
; which when applied to its arguments gives the following result.
;
; 30
