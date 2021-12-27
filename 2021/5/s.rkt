#lang racket

; Day 5: Hydrothermal Venture
; The input is a list of line segments, and we need to compute how many points
; there are where at least two lines overlap. For part A, we need to filter the
; input so we only consider cardinal lines.

(provide today)
(require "../../lib/geom.rkt")

(define/contract (parse lines)
  (-> (listof string?) (listof line?))
  (define/contract (parse-line l)
    (-> string? line?)
    (let ((ps (string-split l " -> ")))
      (line (string->point (first ps)) (string->point (second ps)))))
  (map parse-line lines))

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
      (line->points l)))
  (foldl add-to-heatmap (hash) ls))

(define/contract count-overlaps
  (-> (listof line?) integer?)
  (compose length
           (curry filter (curryr >= 2))
           hash-values
           build-heatmap))

(define solve-a (compose count-overlaps (curry filter line-cardinal?)))
(define solve-b count-overlaps)

(define today (list parse identity solve-a solve-b (const #t)))
