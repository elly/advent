#lang racket

(provide today)
(require "../intcode.rkt"
         "../../lib/list.rkt")

(define (solve-a vm)
  (define (update-screenmap c m)
    (let ((x (first c)) (y (second c)) (p (third c)))
      (hash-set m (cons x y) p)))

  (define (build-screenmap os)
    (foldl update-screenmap (hash) (divide-list os 3)))

  (let ((vm (icvm-copy vm)))
    (let ((sm (with-fixed-io vm '() build-screenmap)))
      (count (curry = 2) (hash-values sm)))))

(define today (list (curryr icvm-load 8192) identity solve-a (const 0)))
