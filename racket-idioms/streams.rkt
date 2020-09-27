#lang racket

(define ones
  (lambda ()
    (cons 1 ones)))

(define (nats)
  (letrec ([f (lambda (x)
		(cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define (powers-of-two)
  (letrec ([f (lambda (x)
		(cons x (lambda () (f (* x 2)))))])
    (f 2)))

(define (make-stream fn arg)
  (letrec ([f (lambda (x)
		(cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define ones2 (make-stream (lambda (x _) x) 1))
(define nats2 (make-stream + 1))
(define powers-of-two2 (make-stream * 2))

((cdr ((cdr (ones2)))))
((cdr ((cdr (nats2)))))
((cdr ((cdr (powers-of-two2)))))
