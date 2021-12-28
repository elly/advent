#lang racket

(provide today)
(require "../../lib/func.rkt"
         "../../lib/geom.rkt"
         "../../lib/vec.rkt")

; Day 25: Sea Cucumbers
;
; We are given a map of sea cucumbers, which move in straight lines either east
; or south. Each step, east-facing sea cucumbers try to move forward atomically,
; then south-facing sea cucumbers try to move forward atomically. If they reach
; the bottom of the map, they wrap around to the top; ditto right to left. We
; want to find the first step where nothing can move.

(define herd? (set/c point?))
(struct state (rows cols east south) #:transparent)

(define/contract (parse ls)
  (-> (listof string?) state?)

  (define/contract (parse-herd ls c)
    (-> (listof string?) char? herd?)
    (let ((vs (line-list->vec2 identity ls)))
      (list->set
        (filter (lambda (i) (equal? (vec2-at vs i) c))
                (vec2-points vs)))))

  (state
    (length ls)
    (string-length (first ls))
    (parse-herd ls #\>)
    (parse-herd ls #\v)))

(define (point+m xm ym p q)
  (let ((xs (+ (point-x p) (point-x q)))
        (ys (+ (point-y p) (point-y q))))
    (point (modulo xs xm) (modulo ys ym) 0)))

(define (move-herd st h d)
  (let ((om (set-union (state-east st) (state-south st))))
    (list->set
      (set-map h
        (lambda (c)
          (let ((nc (point+m (state-cols st) (state-rows st) c d)))
            (if (not (set-member? om nc))
                nc
                c)))))))

(define (step st)
  (let* ((eh (move-herd st (state-east st) (point 1 0 0)))
         (st (struct-copy state st (east eh)))
         (sh (move-herd st (state-south st) (point 0 1 0)))
         (st (struct-copy state st (south sh))))
    st))

(define (step-until-fixed st)
  (add1
    (cdr
      (iterate-until
        (lambda (s) (cons (step (car s)) (add1 (cdr s))))
        (lambda (o n) (equal? (car o) (car n)))
        (cons st 0)))))

(define today (list parse identity step-until-fixed (const #f) (const #t)))
