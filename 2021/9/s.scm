#lang racket

(require "../advent.scm")

; Day 9: Smoke Basin

(define/contract parse
  (-> (listof string?) vec2?)
  (compose list->vector
           (curry map (compose list->vector
                               (curry map (compose string->number string))
                               string->list))))

(define/contract (is-low-point? v p)
  (-> vec2? point2? boolean?)
  (let ((pv (vec2-at v p)))
    (< pv (apply min (map (curry vec2-at v) (vec2-neighbors v p))))))

(define/contract (low-points v)
  (-> vec2? (listof point2?))
  (filter (curry is-low-point? v) (vec2-indexes v)))

(define/contract (risk-level v p)
  (-> vec2? point2? integer?)
  (+ 1 (vec2-at v p)))

(define/contract (low-point-for v p)
  (-> vec2? point2? (or/c point2? #f))
  (cond
    [(= (vec2-at v p) 9) #f]
    [(is-low-point? v p) p]
    [else
      (let ((z (argmin (curry vec2-at v) (vec2-neighbors v p))))
        (low-point-for v z))]))

(define/contract (make-basin-map v)
  (-> vec2? vec2?)
  (vec2-mapi (curry low-point-for v) v))

(define solve
  (fork
    (lambda (v)
      (sum
        (map (curry risk-level v)
             (low-points v))))
    (compose prod
             (curryr take 3)
             (curryr sort >)
             (curry map length)
             (curry group-by identity)
             (curry filter identity)
             vec2-values
             make-basin-map)))

(solve! 9 parse solve)
