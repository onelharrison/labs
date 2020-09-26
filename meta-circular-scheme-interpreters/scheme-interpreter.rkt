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
      [`(+ ,@n-exprs) (apply + (map (lambda (n-expr)
				 (eval-expr n-expr env))
			       n-exprs))]
      [`(* ,@n-exprs) (apply * (map (lambda (n-expr)
				 (eval-expr n-expr env))
			       n-exprs))]
      [`(- ,n1-expr ,@n-rest-exprs) (apply - (cons (eval-expr n1-expr env)
					(map (lambda (n-expr)
					       (eval-expr n-expr env))
					     n-rest-exprs)))]
      ; TODO: Fix implementation - params could be expressions
      ; [`(/ ,n1 ,@(list (? number? n-rest)...)) (apply / (cons n1 n-rest))]
      ; [`(> ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply > (cons n1 (cons n2 n-rest)))]
      ; [`(>= ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply >= (cons n1 (cons n2 n-rest)))]
      ; [`(< ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply < (cons n1 (cons n2 n-rest)))]
      ; [`(<= ,n1 ,n2 ,@(list (? number? n-rest)...)) (apply <= (cons n1 (cons n2 n-rest)))]
      [`(= ,n1-expr ,n2-expr ,@ns-rest-exprs)
	(apply = (cons (eval-expr n1-expr env)
		       (cons (eval-expr n2-expr env)
			     (map (lambda (n)
				    (eval-expr n env)) ns-rest-exprs))))]

      [`(quotient ,n1-expr ,n2-expr) (quotient (eval-expr n1-expr env) (eval-expr n2-expr env))]
      [`(remainder ,n1-expr ,n2-expr) (remainder (eval-expr n1-expr env) (eval-expr n2-expr env))]
      [`(modulo ,n1-expr ,n2-expr) (modulo (eval-expr n1-expr env) (eval-expr n2-expr env))]

      ;; Derived expressions
      ;; -------------------
      ; Binding constructs
      [`(let ((,(? symbol? x) ,x-expr)) ,body)
	((closure x body env) (eval-expr x-expr env))]

      ; TODO: Refactor and add tests
      [`(let* ((,(? symbol? x1) ,x1-expr)
	       (,(? symbol? x2) ,x2-expr)) ,body)
	(eval-expr body (env-extend x2 (eval-expr x2-expr (env-extend x1 (eval-expr x1-expr env) env))
				       (env-extend x1 (eval-expr x1-expr env) env)))]

      [`(letrec ((,(? symbol? x1) ,x1-expr)
	         (,(? symbol? x2) ,x2-expr)) ,body)
          (eval-expr
	    body
	    (env-extend-letrec `((,x1 . ,x1-expr) (,x2 . ,x2-expr)) env))]

      ;; Variable
      ;; --------
      [`,(? symbol? x) (env-lookup x env)]

      ;; Literals
      ;; --------
      [`',datum `,datum]
      [`(quote ,datum) `,datum]
      ['#t #t]
      ['#f #f]
      [`,(? number? n) n]

      ;; Lambda expression
      ;; -----------------
      [`(lambda (,(? symbol? x)) ,body) (closure x body env)]

      ;; Procedure call
      ;; --------------
      [`(,operator ,operand)
	((eval-expr operator env) (eval-expr operand env))]

      ;; Conditionals
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
	(if (eq? var var^) val (env-lookup var env-rest))]
      [`(letrec ,bindings . ,env-rest)
	(match (assq var bindings)
	  [`(,var^ . ,val) (eval-expr val env)]
	  [#f (env-lookup var env-rest)])])))

(define env-extend
  (lambda (var arg env)
   `(ext ,var ,arg . ,env)))

(define env-extend-letrec
  (lambda (bindings env)
    `(letrec ,bindings . ,env)))

(define closure
  (lambda (var body env)
    (lambda (arg)
      (eval-expr body (env-extend var arg env)))))

; TODO: Remove after adding tests
(eval-expr '(let* ((x 1) (y x)) (+ x y)) (empty-env))
