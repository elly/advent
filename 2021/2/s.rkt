#lang racket

(require "../../advent.scm")

(define order? (list/c symbol? integer?))

; Turn the instructions (a list of strings) into a list of parsed orders,
; which are a list of symbol and argument.
(define/contract parse
  (-> (listof string?) (listof order?))
  (curry map
    (compose
        (curryr project s->y s->i)
        (curryr string-split " "))))

; Given an order and an existing Part A state, return the new Part A state.
(define/contract (walk-a i p)
  (-> order? point2? point2?)
  (let ((dir (first i)) (arg (second i))
        (h (first p)) (v (second p)))
    (case dir
      [(up) (list h (- v arg))]
      [(down) (list h (+ v arg))]
      [(forward) (list (+ h arg) v)]
      [else (error i)])))

; Given an order and an existing Part B state, return the new Part B state.
(define/contract (walk-b i p)
  (-> order? point3? point3?)
  (let ((dir (first i)) (arg (second i))
        (h (first p)) (v (second p)) (a (third p)))
    (case dir
      [(up) (list h v (- a arg))]
      [(down) (list h v (+ a arg))]
      [(forward) (list (+ h arg) (+ v (* a arg)) a)]
      [else (error i)])))

(define solve
  (fork
    ; apply all the orders in order, starting at (0 0), then multiply
    (compose prod
             (curry foldl walk-a '(0 0)))
    ; same, but only multiply height & width, not aim
    (compose prod
             (curryr take 2)
             (curry foldl walk-b '(0 0 0)))))

(solve! 2 parse solve)
