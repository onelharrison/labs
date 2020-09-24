#lang scheme

(require "./scheme-interpreter.rkt")

(define-syntax it
  (syntax-rules ()
    [(_ desc actual expected)
     (if (equal? actual expected)
       (displayln (format "it ~s" desc))
       (error 'it (format "Failed! ~s" desc)))]))

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
