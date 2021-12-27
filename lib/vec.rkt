#lang racket

(provide vec2? vec2-inbounds? vec2-at vec2-neighbors vec2-cardinal-neighbors
         vec2-points vec2-mapi vec2-values
         line-list->vec2)
(require "geom.rkt")

(define (vec2? v)
  (and (vector? v)
       (andmap vector? (vector->list v))))

(define (vec2-inbounds? v p)
  (and (>= (point-x p) 0) (< (point-x p) (vector-length (vector-ref v 0)))
       (>= (point-y p) 0) (< (point-y p) (vector-length v))
       (= (point-z p) 0)))

(define (vec2-at v p)
  (vector-ref (vector-ref v (point-y p)) (point-x p)))

(define (vec2-neighbors v p)
  (filter (curry vec2-inbounds? v) (point-neighbors p)))

(define (vec2-cardinal-neighbors v p)
  (filter (and/c (curry vec2-inbounds? v)
                 (or/c (curry points-coplanar-in? 'x p)
                       (curry points-coplanar-in? 'y p)))
          (point-neighbors p)))

(define (vec2-points v)
  (let ((r '()))
    (for ((y (vector-length v)))
      (for ((x (vector-length (vector-ref v y))))
        (set! r (cons (point x y 0) r))))
    (reverse r)))

(define (vec2-mapi f v)
  (build-vector (vector-length v)
    (lambda (y)
      (build-vector (vector-length (vector-ref v y))
                    (lambda (x) (f (point x y 0)))))))

(define (vec2-values v)
  (apply append (map vector->list (vector->list v))))

(define (line-list->vec2 f ls)
  (build-vector (length ls)
    (lambda (y)
      (build-vector (string-length (list-ref ls y))
                    (lambda (x) (f (string-ref (list-ref ls y) x)))))))
