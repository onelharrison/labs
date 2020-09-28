#lang racket

(require racket/fixnum)
(require racket/match)
(require "./utilities.rkt")

; BNF-like Grammar for R0 Concrete Syntax (Racket)
; ================================================
;
; exp ::= int
;       | (read)
;       | (- exp)
;       | (+ exp exp)
; R0  ::= exp
;
; BNF-like Grammar for R0 Abstract Syntax
; =======================================
;
; exp ::= (Int int)
;       | (Prim 'read '())
;       | (Prim '- (list exp))
;       | (Prim '+ (list exp exp))
; R0  ::= (Program '() exp)

; (struct Program (info body))
;
; (struct Int (value))
;
; (struct Prim (op arg))

(define rd
  (Prim 'read '()))

(define neg-eight
  (Prim '- (list (Int 8))))

(define ast1.1
  (Prim '+ (list rd neg-eight)))

(define (leaf? arith)
  (match arith
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list c1)) #f]
    [(Prim '+ (list c1 c2)) #f]))

(leaf? (Prim 'read '())) ; #t
(leaf? (Prim '- (list (Int 8)))) ; #f
(leaf? (Int 8)) ; #t

(define (exp? ast)
  (match ast
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list c1)) (exp? c1)]
    [(Prim '+ (list c1 c2)) (and (exp? c1) (exp? c2))]
    [else #f]))

(define (R0? ast)
  (match ast
    [(Program '() body) (exp? body)]
    [else #f]))

(struct Num (value))

(R0? (Program '() ast1.1)) ; #t
(R0? (Program '()
	      (Prim '- (list (Prim 'read '())
			     (Prim '+ (list (Num 8))))))) ; #f

(define (interp-exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond [(fixnum? r) r]
	   [else (error 'interp-R0 "expected an integer" r)])]
    [(Prim '- (list e))
     (define v (interp-exp e))
     (fx- 0 v)]
    [(Prim '+ (list e1 e2))
     (define v1 (interp-exp e1))
     (define v2 (interp-exp e2))
     (fx+ v1 v2)]))

(define (interp-R0 p)
  (match p
    [(Program '() e) (interp-exp e)]))

; (+ 10 32)
(interp-R0 (Program '()
		    (Prim '+ (list (Int 10) (Int 32))))) ; 42

; (+ 10 (- (+ 12 20)))
(interp-R0 (Program '()
		    (Prim '+ (list (Int 10)
				   (Prim '- (list (Prim '+ (list (Int 12)
								 (Int 20))))))))) ; -22

; (interp-R0 (Program '() ast1.1)) ; 50 => 42

; Partial evaluator for R0

(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(define (pe-R0 p)
  (match p
    [(Program '() e) (Program '() (pe-exp e))]))

(define (test-pe p)
  (assert "testing pe-R0"
	  (equal? (interp-R0 p) (interp-R0 (pe-R0 p)))))

(test-pe (parse-program `(program () (+ 10 (- (+ 5 3))))))
(test-pe (parse-program `(program () (+ 1 (+ 3 1)))))
(test-pe (parse-program `(program () (- (+ 3 (- 5))))))
