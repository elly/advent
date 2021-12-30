#lang racket

(provide today)
(require "../intcode.rkt")

(define (solve-a vm)
  (with-fixed-io vm '(1) last))

(define (solve-b vm)
  (with-fixed-io vm '(5) last))

(define today (list (curryr icvm-load 8192)
                    identity
                    solve-a
                    solve-b (const #t)))
