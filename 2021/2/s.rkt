#lang racket

(require "../../lib/list.rkt")
(provide today)

(define order? (list/c symbol? integer?))

; Turn the instructions (a list of strings) into a list of parsed orders,
; which are a list of symbol and argument.
(define/contract parse
  (-> (listof string?) (listof order?))
  (curry map
    (compose
        (curryr project string->symbol string->number)
        (curryr string-split " "))))

; Given an order and an existing Part A state, return the new Part A state.
(define (walk-a i p)
  (let ((dir (first i)) (arg (second i))
        (h (first p)) (v (second p)))
    (case dir
      [(up) (list h (- v arg))]
      [(down) (list h (+ v arg))]
      [(forward) (list (+ h arg) v)]
      [else (error i)])))

; Given an order and an existing Part B state, return the new Part B state.
(define (walk-b i p)
  (let ((dir (first i)) (arg (second i))
        (h (first p)) (v (second p)) (a (third p)))
    (case dir
      [(up) (list h v (- a arg))]
      [(down) (list h v (+ a arg))]
      [(forward) (list (+ h arg) (+ v (* a arg)) a)]
      [else (error i)])))

(define prod (curry foldl * 1))

(define solve-a
  (compose prod (curry foldl walk-a '(0 0))))
(define solve-b
  (compose prod
           (curryr take 2)
           (curry foldl walk-b '(0 0 0))))

(define today (list parse identity solve-a solve-b (const #t)))
