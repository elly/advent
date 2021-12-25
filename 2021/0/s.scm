#lang racket

(require "../advent.scm")

(solve! 0 first (lambda (a) (cons a a)))
