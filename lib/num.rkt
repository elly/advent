#lang racket

(provide summat)

(define/contract (summat n)
  (-> integer? integer?)
  (/ (* n (+ n 1)) 2))
