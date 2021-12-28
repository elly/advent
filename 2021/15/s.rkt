#lang racket

(provide today)
(require "../../lib/geom.rkt"
         "../../lib/graph.rkt"
         "../../lib/vec.rkt")

; Day 15: Chiton

(define/contract parse
  (-> (listof string?) vec2?)
  (curry line-list->vec2 (compose string->number string)))

(define/contract (shortest-path g s e)
  (-> vec2? point? point? integer?)
  (define (neighbors p)
    (map (lambda (n) (cons n (vec2-at g n)))
         (vec2-cardinal-neighbors g p)))

  (let-values ([(cost path) (dijkstra s e neighbors)])
    cost))

(define/contract (grow g)
  (-> vec2? vec2?)
  (define gr (vector-length g))
  (define gc (vector-length (vector-ref g 0)))

  (define/contract (v-for x y)
    (-> integer? integer? integer?)
    (let ((b (vec2-at g (point (modulo x gc) (modulo y gr) 0)))
          (tx (floor (/ x gc))) (ty (floor (/ y gr))))
      (let ((v (+ b tx ty)))
        (+ 1 (modulo (- v 1) 9)))))

  (build-vector (* gr 5)
    (lambda (y)
      (build-vector (* gc 5)
        (curryr v-for y)))))

(define (spbot g)
  (shortest-path g (point 0 0 0) (vec2-bottom-right g)))

(define solve-a spbot)
(define solve-b (compose spbot grow))

(define today (list parse identity solve-a solve-b (const #t)))
