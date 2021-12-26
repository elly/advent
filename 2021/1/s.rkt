#lang racket

; Sonar Sweep!
; We get given a list of integer depths from the sweep, and we need to compute
; when the depth increases (for part a) and when the sum of the depths over
; a 3-measurement window increases (for part b) - so basically, part b is the
; solution to part a applied to the 3-window rolling sum of the input.

(require "../../advent.rkt")

(define parse (curry map s->i))

; Given a list lst of n integers, return a list r of n - 1 bools, where:
;   r[i] = lst[i] < lst[i + 1]
(define/contract (increasing lst)
  (-> (listof integer?) (listof boolean?))
  (let loop ((prev (car lst)) (rest (cdr lst)) (r '()))
    (cond
      ((null? rest) (reverse r))
      (else
        (loop (car rest)
              (cdr rest)
              (cons (< prev (car rest)) r))))))

; Given a window size w and a list lst of length n, return a list ws of
; length n - w whose elements are lists of length w, such that:
;   ws[n] = (lst[n], lst[n + 1], ..., lst[n + w])
(define/contract (wnds w lst)
  (-> integer? (listof integer?) (listof (listof integer?)))
  (let loop ((ws '()) (lst lst))
    (if (< (length lst) w)
      (reverse ws)
      (loop (cons (take lst w) ws) (cdr lst)))))

(define solve
  (fork
    ; Part A: count the number of true entries in the increasing list for
    ; the input.
    (compose (curry count identity)
             increasing)
    ; Part B: count the number of true entries in the increasinst list for
    ; the list of sums of windows of length 3 of the input.
    (compose (curry count identity)
             increasing
             (curry map sum)
             (curry wnds 3))))

(solve! 1 parse solve)
