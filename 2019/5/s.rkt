#lang racket

(provide today)
(require "../intcode.rkt")

(define today (list (curryr icvm-load 8192)
                    identity
                    (compose (curryr with-fixed-io '(1) last) icvm-copy)
                    (compose (curryr with-fixed-io '(5) last) icvm-copy)))
