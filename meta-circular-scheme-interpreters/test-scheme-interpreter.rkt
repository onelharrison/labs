#lang scheme

(require "./scheme-interpreter.rkt")

(define-syntax it
  (syntax-rules ()
    [(_ desc actual expected)
     (if (equal? actual expected)
       (displayln (format "it ~s" desc))
       (error 'it(format "Failed! ~s: Expected ~s, but got ~s" desc expected actual)))]))

(it "evaluates a number literal to itself"
    (eval-expr '10 (empty-env))
    10)

(it "evaluates a quoted (short-hand) literal the literal itself"
    (eval-expr ''10 (empty-env))
    10)

(it "evaluates a quoted (expanded-form) literal to the literal itself"
    (eval-expr '(quote 10) (empty-env))
    10)

(it "evaluates the #t boolean literal to itself"
    (eval-expr '#t (empty-env))
    #t)

(it "evaluates the #f boolean literal to itself"
    (eval-expr '#f (empty-env))
    #f)

(it "evaluates (boolean? #t) to #t"
    (eval-expr '(boolean? #t) (empty-env))
    #t)

(it "evaluates (boolean? #f) to #t"
    (eval-expr '(boolean? #f) (empty-env))
    #t)

(it "evaluates (boolean? n) to #f where n is a number"
    (eval-expr '(boolean? 10) (empty-env))
    #f)

(it "evaluates (number? n) where n is a complex number"
    (eval-expr '(number? 3+4i) (empty-env))
    #t)

(it "evaluates (number? n) where n is a real number"
    (eval-expr '(number? 3.14) (empty-env))
    #t)

(it "evaluates (number? n) where n is a rational number"
    (eval-expr '(number? 3/10) (empty-env))
    #t)

(it "evaluates (number? n) where n is an integer"
    (eval-expr '(number? 10) (empty-env))
    #t)

(it "evaluates (complex? n) where n is a complex number"
    (eval-expr '(complex? 3+4i) (empty-env))
    #t)

(it "evaluates (real? n) where n is a real number"
    (eval-expr '(real? 3.14) (empty-env))
    #t)

(it "evaluates (rational? n) where n is a rational number"
    (eval-expr '(rational? 3/10) (empty-env))
    #t)

(it "evaluates (integer? n) where n is an integer"
    (eval-expr '(integer? 10) (empty-env))
    #t)

(it "evaluates (+) to 0"
    (eval-expr '(+) (empty-env))
    0)

(it "evaluates (+ 1) to 1"
    (eval-expr '(+ 1) (empty-env))
    1)

(it "evaluates (+ 1 2) to 3"
    (eval-expr '(+ 1 2) (empty-env))
    3)

(it "evaluates (+ 1 2 3) to 6"
    (eval-expr '(+ 1 2 3) (empty-env))
    6)

(it "evaluates (*) to 1"
    (eval-expr '(*) (empty-env))
    1)

(it "evaluates (* 0) to 0"
    (eval-expr '(* 0) (empty-env))
    0)

(it "evaluates (* 1) to 1"
    (eval-expr '(* 1) (empty-env))
    1)

(it "evaluates (* 1 2) to 2"
    (eval-expr '(* 1 2) (empty-env))
    2)

(it "evaluates (* 1 2 3) to 6"
    (eval-expr '(* 1 2 3) (empty-env))
    6)

(it "evaluates (- 1) to -1"
    (eval-expr '(- 1) (empty-env))
    -1)

(it "evaluates (- 3 2) to 1"
    (eval-expr '(- 3 2) (empty-env))
    1)

(it "evalutes (> 3 2) to #t"
    (eval-expr '(> 3 2) (empty-env))
    #t)

(it "evalutes (> 3 2 1) to #t"
    (eval-expr '(> 3 2 1) (empty-env))
    #t)

(it "evalutes (>= 3 2) to #t"
    (eval-expr '(>= 3 2) (empty-env))
    #t)

(it "evalutes (>= 2 2 1) to #t"
    (eval-expr '(>= 2 2 1) (empty-env))
    #t)

(it "evalutes (< 1 2) to #t"
    (eval-expr '(< 1 2) (empty-env))
    #t)

(it "evalutes (< 1 2 3) to #t"
    (eval-expr '(< 1 2 3) (empty-env))
    #t)

(it "evalutes (<= 1 2) to #t"
    (eval-expr '(<= 1 2) (empty-env))
    #t)

(it "evalutes (<= 1 2 2) to #t"
    (eval-expr '(<= 1 2 2) (empty-env))
    #t)

(it "evalutes (= 2 2) to #t"
    (eval-expr '(= 2 2) (empty-env))
    #t)

(it "evalutes (= 2 2 2) to #t"
    (eval-expr '(= 2 2 2) (empty-env))
    #t)

(it "evalutes (quotient 10 3) to 3"
    (eval-expr '(quotient 10 3) (empty-env))
    3)

(it "evalutes (remainder 10 3) to 1"
    (eval-expr '(remainder 10 3) (empty-env))
    1)

(it "evalutes (modulo 10 3) to 1"
    (eval-expr '(modulo 10 3) (empty-env))
    1)
