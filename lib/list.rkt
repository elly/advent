#lang racket

(provide project)

(define (project l . fs)
  (map (lambda (f e) (f e)) fs l))
