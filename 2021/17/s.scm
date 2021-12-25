#lang racket

(require "../advent.scm")

; Day 17: Trick Shot
; We are launching a probe with an initial x,y velocity pair and trying to
; ensure that at some simulation step it is within a target area.
;
; The input is just one line, like:
;   target area: x=20..30, y=-10..-5
; which we turn into a rect:

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

  (let ((ps (map s->i (string-split (strip (first ls)) " "))))
    (make-rect (list (first ps) (third ps))
               (list (second ps) (fourth ps)))))

; The probe has a position and a velocity:
(define probe? (list/c point2? point2?))
(define probe-pos first)
(define probe-vel second)

; And to step it we:
(define/contract (step p)
  (-> probe? probe?)
  (list
    (point+ (first p) (second p))
    (let* ((v (second p))
           (vx (first v)) (vy (second v)))
      (list
        (if (= vx 0) 0 (- vx (/ vx (abs vx))))
        (sub1 vy)))))

(define/contract (done? pr r)
  (-> probe? rect? boolean?)
  (or (and (< (second (first pr)) (rect-bottom r))
           (<= (second (second pr)) 0))
      (rect-contains? r (first pr))))

(define/contract (sim r v)
  (-> rect? point2? (or/c point2? #f))
  (let loop ((pr (list '(0 0) v)))
    (if (done? pr r)
        (if (rect-contains? r (first pr))
            v
            #f)
        (loop (step pr)))))

(define/contract (max-height v)
  (-> point2? integer?)
  (let ((y (second v)))
    (/ (* y (add1 y)) 2)))

(define/contract (all-vectors r)
  (-> rect? (listof point2?))
  (filter (curry sim r)
          (cartesian-product
            (build-list (first (second r)) identity)
            (build-list 200 (curryr - 100)))))

(define solve
  (fork
    (compose max-height (curry argmax max-height) all-vectors)
    (compose length all-vectors)))

(solve! 17 parse solve)
