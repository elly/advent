#lang racket

(provide today)
(require "../intcode.rkt")

(define today (list (curryr icvm-load 8192)
                    identity
                    (curryr with-fixed-io '(1) last)
                    (curryr with-fixed-io '(5) last)))
