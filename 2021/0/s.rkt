#lang racket

(provide today)

(define parse (curry map string->number))
(define extract (curryr sort <))
(define solve-a first)
(define solve-b last)
(define test (const #f))

(define today (list parse extract solve-a solve-b test))
