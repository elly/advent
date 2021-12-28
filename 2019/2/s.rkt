#lang racket

(provide today)
(require "../intcode.rkt")

(define parse
  (compose make-icvm
           list->vector
           (curry map string->number)
           (curryr string-split ",")
           car))

(define extract identity)

(define (run-with vm a b)
  (let ((vm (icvm-copy vm)))
    (icvm-memset! vm 1 a)
    (icvm-memset! vm 2 b)
    (icvm-run! vm)
    (icvm-memref vm 0)))

(define (solve-a vm) (run-with vm 12 2))

(define (solve-b vm)
  (ormap
    (lambda (vs)
      (if (= (run-with vm (first vs) (second vs)) 19690720)
          (+ (* 100 (first vs)) (second vs))
          #f))
    (cartesian-product (build-list 100 identity)
                       (build-list 100 identity))))

(define today (list parse extract solve-a solve-b (const #t)))
