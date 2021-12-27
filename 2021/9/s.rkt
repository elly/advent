#lang racket

; Day 9: Smoke Basin
(provide today)
(require "../../lib/geom.rkt"
         "../../lib/list.rkt"
         "../../lib/vec.rkt")

(define/contract parse
  (-> (listof string?) vec2?)
  (curry line-list->vec2 (compose string->number string)))

(define/contract (is-low-point? v p)
  (-> vec2? point? boolean?)
  (let ((pv (vec2-at v p)))
    (< pv (apply min (map (curry vec2-at v) (vec2-cardinal-neighbors v p))))))

(define/contract (low-points v)
  (-> vec2? (listof point?))
  (filter (curry is-low-point? v) (vec2-points v)))

(define/contract (risk-level v p)
  (-> vec2? point? integer?)
  (+ 1 (vec2-at v p)))

(define/contract (low-point-for v p)
  (-> vec2? point? (or/c point? #f))
  (cond
    [(= (vec2-at v p) 9) #f]
    [(is-low-point? v p) p]
    [else
      (let ((z (argmin (curry vec2-at v) (vec2-cardinal-neighbors v p))))
        (low-point-for v z))]))

(define/contract (make-basin-map v)
  (-> vec2? vec2?)
  (vec2-mapi (curry low-point-for v) v))

(define (solve-a e)
  (sum (map (curry risk-level e) (low-points e))))

(define solve-b
  (compose prod
           (curryr take 3)
           (curryr sort >)
           (curry map length)
           (curry group-by identity)
           (curry filter identity)
           vec2-values
           make-basin-map))

(define today (list parse identity solve-a solve-b (const #t)))
