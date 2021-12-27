#lang racket

(provide today)
(require "../../lib/geom.rkt"
         "../../lib/vec.rkt")

; Day 15: Chiton

(define/contract parse
  (-> (listof string?) vec2?)
  (compose list->vector
           (curry map
              (compose list->vector
                       (curry filter integer?)
                       (curry map string->number)
                       (curryr string-split "")))))

(define/contract (shortest-path g s e)
  (-> vec2? point? point? integer?)

  (define/contract (make-vec2-hash g v)
    (-> vec2? any/c hash?)
    (make-hash (map (lambda (i) (cons i v)) (vec2-points g))))

  (define/contract (dijkstra g s e)
    (-> vec2? point? point? (cons/c hash? hash?))
    (define dm (make-vec2-hash g 999999999))
    (define pm (make-vec2-hash g #f))
    (hash-set! dm s 0)
    ; Note to self: this is likely "the slow part" since q is the size of the
    ; input!
    (let loop ((q (list->set (vec2-points g))))
      (if (set-empty? q)
        (cons dm pm)
        (let* ((u (argmin (curry hash-ref dm) (set->list q)))
               (du (hash-ref dm u))
               (vs (filter (curry set-member? q)
                           (vec2-cardinal-neighbors g u))))
          (if (equal? e u)
            (cons dm pm)
            (let iloop ((vs vs))
              (if (null? vs)
                  (loop (set-remove q u))
                  (let* ((v (car vs))
                         (a (+ du (vec2-at g v))))
                    (if (< a (hash-ref dm v))
                      (begin
                        (hash-set! dm v a)
                        (hash-set! pm v u)
                        (iloop (cdr vs)))
                      (iloop (cdr vs)))))))))))

  (let ((m (dijkstra g s e)))
    (hash-ref (car m) e)))

; Some notes to myself about part b:
; The map expands - the original map is the top-left tile of a map of 5x5 tiles,
; and the danger values of subsequent tiles change. Specifically, for the tile
; at (Tx,Ty):
;   v[Tx,Ty,x,y] = 1 + (v[0,0,x,y] + x + y - 1) % 9
; We won't be able to run the existing solution on it - it takes over 10 seconds
; on the 100x100 input, although maybe that can be improved using mutation
; and vectors. That's probably a good first place to start because if it works
; it will save doing anything clever :)
;
; Just kidding, I ran it overnight and it worked.

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
