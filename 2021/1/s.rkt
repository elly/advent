#lang racket

; Sonar Sweep!
; We get given a list of integer depths from the sweep, and we need to compute
; when the depth increases (for part a) and when the sum of the depths over
; a 3-measurement window increases (for part b) - so basically, part b is the
; solution to part a applied to the 3-window rolling sum of the input.

(provide today)
(require "../../lib/list.rkt")

(define parse (curry map string->number))

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

(define extract identity)
(define solve-a (compose (curry count identity) increasing))
(define solve-b (compose (curry count identity)
                         increasing
                         (curry map sum)
                         (curry wnds 3)))

(define today (list parse extract solve-a solve-b (const #t)))
