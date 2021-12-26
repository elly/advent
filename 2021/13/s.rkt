#lang racket

(require "../../advent.rkt")

; Day 13: Transparent Origami
; We are given a set of points and then a list of lines to fold along,
; where folding mirrors every point after the fold line to the space before it.
;
; For example, after folding (0,14) across y=7, it ends up at (0,0), and after
; folding (9,0) across x=5, it ends up at (1,0)

(define fold? (list/c symbol? integer?))
(define spec? (list/c (listof point2?) (listof fold?)))

(define/contract (parse ls)
  (-> (listof string?) spec?)

  (define/contract (parse-point l)
    (-> string? (or/c point2? boolean?))
    (let ((lp (string-split l ",")))
      (if (= (length lp) 2)
          (project lp s->i s->i)
          #f)))

  (define/contract (parse-fold l)
    (-> string? (or/c fold? boolean?))
    (let ((lp (string-split l " ")))
      (if (= (length lp) 3)
        (project (string-split (third lp) "=") s->y s->i)
        #f)))

  (list
    (filter-map parse-point ls)
    (filter-map parse-fold ls)))

(define/contract (fold-point-across f p)
  (-> fold? point2? point2?)
  (let ((ft (first f)) (fc (second f)) (x (first p)) (y (second p)))
    (cond
      [(symbol=? ft 'y)
       (list x
             (if (> y fc)
                 (- fc (- y fc))
                 y))]
      [(symbol=? ft 'x)
       (list
             (if (> x fc)
                (- fc (- x fc))
                x)
             y)]
      [else (error "?" f p)])))

(define/contract (apply-fold f ps)
  (-> fold? (set/c point2?) (set/c point2?))
  (list->set (set-map ps (curry fold-point-across f))))

(define (plot ps)
  (for ([y 7])
    (write (list->string
      (build-list
        41
        (lambda (x) (if (set-member? ps (list x y)) #\# #\space)))))
    (newline))
  #t)

(define solve
  (fork
    (lambda (sp)
      (set-count (apply-fold (first (second sp)) (list->set (first sp)))))
    (lambda (sp)
      (plot
         (foldl apply-fold (list->set (first sp)) (second sp))))))

(solve! 13 parse solve)
