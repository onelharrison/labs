#lang scheme

(require scheme/match)

; <expression> ::= <variable>
;                | <literal>
;                | <procedure call>
;                | <lambda expression>
;                | <conditional>
;                | <assignment>
;                | <derived expression>
;                | <macro use>
;                | <macro block>

; <literal> ::= <quotation>
;             | <self-evaluating>
;
; <quotation> ::= '<datum>
;               | (quote <datum>)
;
; <datum> ::= ...
;
; <self-evaluating ::= <boolean>
;                    | <number>
;                    | <character>
;                    | <string>
;
; <boolean> ::= #t | #f
; <number> ::= ...
; <character> ::= ...
; <string> ::= ...
;
; <conditional> ::= ...
; <assignment> ::= ...
; <derived expression> ::= ...
; <macro use> ::= ...
; <macro block> ::= ...
;
; <procedure call> ::= (<operator> <operand>*)
; <operator> ::= <expression>
; <operand> ::= <expression>

(provide eval-expr empty-env)

(define eval-expr
  (lambda (expr env)
    (match expr
      ;; built-in procedures
      ;; -------------------
      ; boolean procedures
      [`(boolean? ,b) (boolean? b)]

      ; numerical procedures
      [`(number? ,n) (number? n)]
      [`(complex? ,n) (complex? n)]
      [`(real? ,n) (real? n)]
      [`(rational? ,n) (rational? n)]
      [`(integer? ,n) (integer? n)]

      ;; variable
      ;; --------
      [`,(? symbol? x) (env-lookup x env)]

      ;; literals
      ;; --------
      [`',datum `,datum]
      [`(quote ,datum) `,datum]
      ['#t #t]
      ['#f #f]
      [`,(? number? n) n]

      ;; lambda expression
      ;; -----------------
      [`(lambda (,(? symbol? x)) ,body) (closure eval-expr x body env)]

      ;; procedure call
      ;; --------------
      [`(,operator ,operand)
	((eval-expr operator env) (eval-expr operand env))]

      ;; conditionals
      ;; ------------
      [`(if ,test-expr ,consequent-expr ,alternate-expr)
	(if (eval-expr test-expr env)
	  (eval-expr consequent-expr env)
	  (eval-expr alternate-expr env))])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define env-lookup
  (lambda (var env)
    (match env
      [`(empty-env) (error 'env-lookup "unbound variable ~s" var)]
      [`(ext ,var^ ,val . ,env-rest)
	(if (eq? var var^) val (env-lookup var env-rest))])))

(define env-extend
  (lambda (var arg env)
   `(ext ,var ,arg . ,env)))

(define closure
  (lambda (evaluator var body env)
    (lambda (arg)
      (evaluator body (env-extend var arg env)))))
