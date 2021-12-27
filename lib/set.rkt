#lang racket

(provide set-filter)

(define (set-filter f s)
  (for/set ((e s) #:when (f e)) e))
