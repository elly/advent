#lang racket

(require rackunit "advent.scm")

(check-equal? (s->i "3") 3)
(check-equal? (s->ca "abc") #(#\a #\b #\c))

(check-equal? (sum '(3 4 5)) 12)
(check-equal? ((fork add1 sub1) 2) (cons 3 1))

(check-equal? (set-count (list->set (rotations-of '(1 2 3))))
              24)
(check-equal? (set-count (list->set (rotations-of '(1 1 1))))
              8)

(check-equal? (nth-rotation-of 0 '(1 2 3)) '(1 2 3))
