#lang racket

; Day 17: Trick Shot
; We are launching a probe with an initial x,y velocity pair and trying to
; ensure that at some simulation step it is within a target area.
;
; The input is just one line, like:
;   target area: x=20..30, y=-10..-5
; which we turn into a rect:

(provide today)
(require "../../lib/func.rkt"
         "../../lib/geom.rkt"
         "../../lib/num.rkt")

(define/contract (parse ls)
  (-> (listof string?) rect?)

  (define/contract strip
    (-> string? string?)
    (compose
      (curryr string-replace "target area: " "")
      (curryr string-replace "x=" "")
      (curryr string-replace "y=" "")
      (curryr string-replace "," "")
      (curryr string-replace ".." " ")))

  (let ((ps (map string->number (string-split (strip (first ls)) " "))))
    (rect (point (first ps) (third ps) 0)
          (point (second ps) (fourth ps) 0))))

; The probe has a position and a velocity:
(struct probe (pos vel))

; And to step it we:
(define/contract (step p)
  (-> probe? probe?)
  (probe
    (point+ (probe-pos p) (probe-vel p))
    (let* ((v (probe-vel p))
           (vx (point-x v)) (vy (point-y v)))
      (point
        (if (= vx 0) 0 (- vx (/ vx (abs vx))))
        (sub1 vy)
        0))))

(define/contract (done? pr r)
  (-> probe? rect? boolean?)
  (or (and (< (point-y (probe-pos pr))
              (point-y (rect-min r)))
           (<= (point-y (probe-vel pr)) 0))
      (rect-contains? r (probe-pos pr))))

(define (sim r v)
  (rect-contains? r
    (probe-pos
      (iterate-until
        step
        (lambda (e n) (done? e r))
        (probe *origin-point* v)))))

(define max-height (compose summat point-y))

(define/contract (all-vectors r)
  (-> rect? (listof point?))
  (filter (curry sim r)
          (map (lambda (l) (point (first l) (second l) 0))
               (cartesian-product
                  (build-list (add1 (point-x (rect-max r))) identity)
                  (build-list 200 (curryr - 100))))))

(define extract all-vectors)

(define solve-a
  (compose max-height (curry argmax max-height)))

(define today (list parse all-vectors solve-a length (const #t)))
