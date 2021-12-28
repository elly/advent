#lang racket

(provide today)
(require "../../lib/list.rkt")

(define (fuel-cost m)
  (max (- (floor (/ m 3)) 2)
       0))

(define (rec-fuel-cost m)
  (let loop ((m m) (t 0) (d (fuel-cost m)))
    (if (= d 0)
        t
        (loop d (+ t d) (fuel-cost d)))))

(define parse (curry map string->number))
(define solve-a
  (compose sum (curry map fuel-cost)))
(define solve-b
  (compose sum (curry map rec-fuel-cost)))

(define today (list parse identity solve-a solve-b (const #t)))
