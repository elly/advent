#lang racket

(provide today)
(require "../../lib/geom.rkt"
         "../../lib/list.rkt")

; Day 13: Transparent Origami
; We are given a set of points and then a list of lines to fold along,
; where folding mirrors every point after the fold line to the space before it.
;
; For example, after folding (0,14) across y=7, it ends up at (0,0), and after
; folding (9,0) across x=5, it ends up at (1,0)

(define fold? (list/c symbol? integer?))
(define spec? (cons/c (set/c point?) (listof fold?)))

(define/contract (parse ls)
  (-> (listof string?) spec?)

  (define/contract (parse-fold l)
    (-> string? (or/c fold? boolean?))
    (let ((lp (string-split l " ")))
      (if (= (length lp) 3)
        (project (string-split (third lp) "=") string->symbol string->number)
        #f)))

  (cons
    (list->set (filter-map string->point ls))
    (filter-map parse-fold ls)))

(define (fold-point-across f p)
  (define (flip v p) (if (> v p) (- p (- v p)) v))
  (case (first f)
    [(y) (struct-copy point p (y (flip (point-y p) (second f))))]
    [(x) (struct-copy point p (x (flip (point-x p) (second f))))]
    [else (error "?" f p)]))

(define (apply-fold f ps)
  (list->set (set-map ps (curry fold-point-across f))))

(define (plot ps)
  (for ([y 7])
    (write (list->string
      (build-list
        41
        (lambda (x) (if (set-member? ps (point x y 0)) #\# #\space)))))
    (newline))
  #t)

(define (solve-a in)
  (set-count (apply-fold (first (cdr in)) (car in))))

(define (solve-b in)
  (let ((r (foldl apply-fold (car in) (cdr in))))
    (plot r)
    "HECRZKPR"))    ; nasty

(define today (list parse identity solve-a solve-b (const #t)))
