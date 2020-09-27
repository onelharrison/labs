#lang racket

(define fibonacci
  (letrec ([memo `((2 . 1) (1 . 0))]
	   [f (lambda (n)
		(if (or (zero? n) (negative? n))
		  (error 'fibonacci "undefined for n <= 0")
		  (let ([ans (assoc n memo)])
                    (if ans
                      (cdr ans)
                      (let ([new-ans (+ (f (- n 1)) (f (- n 2)))])
                        (begin
	                  (set! memo (cons `(,n . ,new-ans) memo))
	                  new-ans))))))])
    f))

(fibonacci 100000)
