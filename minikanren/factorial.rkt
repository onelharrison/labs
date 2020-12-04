#lang racket

(require minikanren)
(require minikanren/numbers)


(define zo (build-num 0))
(define 1o (build-num 1))

(define (factorialo n f)
   (fresh ()
     (<=o zo n)
     (<=o n f)
     (conde
       [(zeroo n) (== f 1o)]
       [(== 1o n) (== f 1o)]
       [(>1o n)
        (fresh (f-1 n-1o)
          (*o n f-1 f)
          (minuso n 1o n-1o)
          (factorialo  n-1o f-1))])))

; Question: How does the factorialo procedure's time-to-first-answer grow w.r.t. n?
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 0) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 1) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 2) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 3) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 4) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 5) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 6) q)))
;
; (time
;   (run 1 (q)
;     (factorialo (build-num 7) q)))
;
; Raw Results - Trial #1
; ----------------------
; cpu time: 14 real time: 14 gc time: 0
; cpu time: 1 real time: 0 gc time: 0
; cpu time: 6 real time: 6 gc time: 0
; cpu time: 4 real time: 3 gc time: 0
; cpu time: 6 real time: 5 gc time: 0
; cpu time: 51 real time: 50 gc time: 0
; cpu time: 9989 real time: 9942 gc time: 1071
; cpu time: 1253850 real time: 1229370 gc time: 492418
;
; Raw Results - Trial #2
; ----------------------
; cpu time: 6 real time: 6 gc time: 0
; cpu time: 0 real time: 0 gc time: 0
; cpu time: 2 real time: 2 gc time: 0
; cpu time: 2 real time: 2 gc time: 0
; cpu time: 1 real time: 1 gc time: 0
; cpu time: 20 real time: 20 gc time: 0
; cpu time: 5203 real time: 5190 gc time: 604
; cpu time: 504173 real time: 500397 gc time: 198130
;
; Raw Results - Trial #3
; ----------------------
; cpu time: 10 real time: 10 gc time: 0
; cpu time: 1 real time: 0 gc time: 0
; cpu time: 5 real time: 5 gc time: 0
; cpu time: 2 real time: 2 gc time: 0
; cpu time: 5 real time: 5 gc time: 0
; cpu time: 63 real time: 63 gc time: 0
; cpu time: 15090 real time: 14794 gc time: 1486
; cpu time: 721338 real time: 710307 gc time: 281233
;
; Notes:
; - 🤔 the start time of the first factorialo calls is interesting
;
; minikanren proof of factorial as a mathematical function
;
; -- zo
(run 1 (q)
  (factorialo zo q)) ; terminates with one output

(run 2 (q)
  (factorialo zo q)) ; terminates with one output

(run* (q)
  (factorialo zo q)) ; terminates with one output

; -- 1o
(run 1 (q)
  (factorialo 1o q)) ; terminates with one output

(run 2 (q)
  (factorialo 1o q)) ; terminates with one output

(run* (q)
  (factorialo 1o q)) ; terminates with one output

; -- 2o
(run 1 (q)
  (factorialo (build-num 2) q)) ; terminates with one output

(run 2 (q)
  (factorialo (build-num 2) q)) ; terminates with one output

(run* (q)
  (factorialo (build-num 2) q)) ; terminates with one output

; -- 3o (get funky)
(run 1 (q)
  (factorialo (build-num 3) q)) ; terminates with one output

(run 2 (q)
  (factorialo (build-num 3) q)) ; long-running or non-terminating behavior

(run* (q)
  (factorialo (build-num 3) q)) ; long-running or non-terminating behavior

; Notes:
; - conjecture: long-runing or non-terminating behavior will be observed for
;                `(run n ...)` where n in [3, +inf)
;
;   🤔 why does this (^) happen? Given the behavior for zo, 1o, and 2o,
;      long-running behavior that exceeds the expected time-to-first answer is not expected in 3+o
;      and the procedure is expected to terminate.
