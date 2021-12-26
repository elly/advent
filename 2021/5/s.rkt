#lang racket

; Day 5: Hydrothermal Venture
; The input is a list of line segments, and we need to compute how many points
; there are where at least two lines overlap. For part A, we need to filter the
; input so we only consider cardinal lines.

(require "../../advent.rkt")

(define (line? v)
  (list/c integer? integer? integer? integer?))

(define/contract (line-start l)
  (-> line? point2?)
  (take l 2))

(define/contract (line-end l)
  (-> line? point2?)
  (drop l 2))

(define/contract (parse lines)
  (-> (listof string?) (listof line?))
  (define/contract (parse-line line)
    (-> string? line?)
    (let* ((ps (string-split line " -> "))
           (lps (string-split (first ps) ","))
           (rps (string-split (second ps) ",")))
      (map s->i (list (first lps) (second lps) (first rps) (second rps)))))
  (map parse-line lines))

(define/contract (points-on-line-segment l)
  (-> line? (listof point2?))
  (let* ((s (line-start l))
         (e (line-end l))
         (d (unitize (point- e s))))
    (let loop ((r '()) (p s))
      (if (equal? p e)
        (reverse (cons p r))
        (loop (cons p r) (point+ p d))))))

; Given a list of line segments, builds a map from point to number of segments
; that overlap that point.
(define/contract (build-heatmap ls)
  (-> (listof line?) hash?)
  (define/contract (add-to-heatmap l m)
    (-> line? hash? hash?)
    (foldl
      (lambda (p ms)
        (hash-update ms p add1 0))
      m
      (points-on-line-segment l)))
  (foldl add-to-heatmap (hash) ls))

(define/contract (cardinal? line)
  (-> line? boolean?)
  (let ((d (unitize (point- (line-end line) (line-start line)))))
    (= 1 (length (filter (curryr (negate eq?) 0) d)))))

(define/contract count-overlaps
  (-> (listof line?) integer?)
  (compose length
           (curry filter (curryr >= 2))
           hash-values
           build-heatmap))

(define solve
  (fork
    (compose count-overlaps
             (curry filter cardinal?))
    count-overlaps))

(solve! 5 parse solve)
