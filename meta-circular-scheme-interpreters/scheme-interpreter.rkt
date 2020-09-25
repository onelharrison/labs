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
      [`(boolean? ,b-expr) (boolean? (eval-expr b-expr env))]

      ; numerical procedures
      [`(number? ,n-expr) (number? (eval-expr n-expr env))]
      [`(complex? ,n-expr) (complex? (eval-expr n-expr env))]
      [`(real? ,n-expr) (real? (eval-expr n-expr env))]
      [`(rational? ,n-expr) (rational? (eval-expr n-expr env))]
      [`(integer? ,n-expr) (integer? (eval-expr n-expr env))]
      [`(+ ,@ns) (apply + (map (lambda (n)
				 (eval-expr n env))
			       ns))]
      [`(* ,@ns) (apply * (map (lambda (n)
				 (eval-expr n env))
			       ns))]
      [`(- ,n1 ,@n-rest) (apply - (cons (eval-expr n1 env)
					(map (lambda (n)
					       (eval-expr n env))
					     n-rest)))]
      ; TODO: Fix implementation - params could be expressions
      ; [`(/ ,n1 ,@(list (? number? n-rest)...)) (apply / (cons n1 n-rest))]
      ; [`(> ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply > (cons n1 (cons n2 n-rest)))]
      ; [`(>= ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply >= (cons n1 (cons n2 n-rest)))]
      ; [`(< ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply < (cons n1 (cons n2 n-rest)))]
      ; [`(<= ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply <= (cons n1 (cons n2 n-rest)))]
      ; [`(= ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply = (cons n1 (cons n2 n-rest)))]
      [`(quotient ,n1-expr ,n2-expr) (quotient (eval-expr n1-expr env) (eval-expr n2-expr env))]
      [`(remainder ,n1-expr ,n2-expr) (remainder (eval-expr n1-expr env) (eval-expr n2-expr env))]
      [`(modulo ,n1-expr ,n2-expr) (modulo (eval-expr n1-expr env) (eval-expr n2-expr env))]

      ;; Derived expressions
      ;; -------------------
      ; Binding constructs
      [`(let ((,(? symbol? x) ,x-expr)),body)
	((closure eval-expr x body env) (eval-expr x-expr env))]

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
