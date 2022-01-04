#lang racket

(provide today)
(require "../intcode.rkt"
         "../../lib/list.rkt")

(define (tile->glyph t)
  (case t
   ((0) #\space)
   ((1) #\#)
   ((2) #\=)
   ((3) #\-)
   ((4) #\*)))

(define (raster sm)
  (for ((y 24))
    (for ((x 79))
      (let ((t (hash-ref sm (cons x y) 0)))
        (printf "~a" (tile->glyph t))))
      (printf "~n"))
  (printf "score: ~a~n" (hash-ref sm (cons -1 0) 0)))

(define (update-screenmap c m)
  (let ((x (first c)) (y (second c)) (p (third c)))
    (hash-set m (cons x y) p)))

(define (build-screenmap os)
  (foldl update-screenmap (hash) (divide-list os 3)))

(define (solve-a vm)
  (let ((vm (icvm-copy vm)))
    (let ((sm (with-fixed-io vm '() build-screenmap)))
      (count (curry = 2) (hash-values sm)))))

(define (solve-b vm)
  (define (prep-vm! vm)
    (icvm-memset! vm 0 2))

  (define (win-game vm) 0)

  (let ((vm (icvm-copy vm)))
    (prep-vm! vm)
    (raster (build-screenmap (win-game vm)))
    0))

(define today (list (curryr icvm-load 8192) identity solve-a solve-b))
