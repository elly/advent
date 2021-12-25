#lang racket

(require "../../advent.scm")

; Day 6: Lanternfish
; Each lanternfish creates a new lanternfish every 7 days.
; We can model each fish as a single number: days until it creates a new
; lanternfish.
; A new lanternfish needs two days longer for its first cycle.
; Lanternfish spawn a new fish if their timer is 0 at the start of the day.

; Each day, a 0 becomes a 6 and adds a new 8 to the end, while each other
; number decreases by one.

(define *max-fish* 9)

; We store a vector of the number of fish with each count:
(define/contract (build-fvec fs)
  (-> (listof integer?) (vectorof integer?))
  (build-vector *max-fish*
               (lambda (i) (count (curry = i) fs))))

(define/contract parse
  (-> (listof string?) (vectorof integer?))
  (compose build-fvec
           (curry map s->i)
           (curryr string-split ",")
           car))

(define/contract (step-fvec fv)
  (-> (vectorof integer?) (vectorof integer?))
  (define nf (curry vector-ref fv))
  (build-vector *max-fish*
    (lambda (i)
      (cond
        ; 7 or 0 -> 6, 0 -> 8, everything else decrements
        [(= i 6) (+ (nf 0) (nf 7))]
        [(= i 8) (nf 0)]
        [else (nf (add1 i))]))))

(define solve
  (fork
    (compose sum vector->list (curry iterate-n step-fvec 80))
    (compose sum vector->list (curry iterate-n step-fvec 256))))

(solve! 6 parse solve)
