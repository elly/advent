#lang racket

(require "../advent.scm")

; Day 25: Sea Cucumbers
;
; We are given a map of sea cucumbers, which move in straight lines either east
; or south. Each step, east-facing sea cucumbers try to move forward atomically,
; then south-facing sea cucumbers try to move forward atomically. If they reach
; the bottom of the map, they wrap around to the top; ditto right to left. We
; want to find the first step where nothing can move.

(define point? (cons/c integer? integer?))
(define herd? (set/c point?))
(struct state (rows cols east south) #:transparent)

(define/contract (parse ls)
  (-> (listof string?) state?)

  (define/contract (parse-herd ls c)
    (-> (listof string?) char? herd?)
    (let ((vs (list->vec2 (map string->list ls))))
      (list->set
        (filter-map (lambda (i) (and (equal? (vec2-at vs i) c)
                                     (cons (first i) (second i))))
                    (vec2-indexes vs)))))

  (state
    (length ls)
    (string-length (first ls))
    (parse-herd ls #\>)
    (parse-herd ls #\v)))

(define (point+m xm ym p q)
  (cons (modulo (+ (car p) (car q)) xm)
        (modulo (+ (cdr p) (cdr q)) ym)))

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
  (let* ((eh (move-herd st (state-east st) (cons 1 0)))
         (st (struct-copy state st (east eh)))
         (sh (move-herd st (state-south st) (cons 0 1)))
         (st (struct-copy state st (south sh))))
    st))

(define (step-until-fixed st)
  (let loop ((n 0) (st st))
    (let ((nst (step st)))
      (if (equal? st nst)
          (add1 n)
          (loop (add1 n) nst)))))

(define solve
  (fork
    step-until-fixed
    (const 0)))

(solve! 25 parse solve)
