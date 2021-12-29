#lang racket

(provide today)
(require "../../lib/geom.rkt"
         "../../lib/list.rkt")

(define wire? (listof (cons/c symbol? integer?)))

(define/contract (parse ls)
  (-> (listof string?) (listof wire?))

  (define (parse-wire-seg s)
    (let ((d (substring s 0 1))
          (a (substring s 1)))
      (cons (string->symbol d) (string->number a))))
  (define parse-wire
    (compose (curry map parse-wire-seg)
             (curryr string-split ",")))
  (map parse-wire ls))

(define/contract (sim w)
  (-> wire? (listof point?))

  (define (move p d)
    (point+ p
      (case (car d)
        ((U) (point 0 (* -1 (cdr d)) 0))
        ((D) (point 0 (cdr d) 0))
        ((L) (point (* -1 (cdr d)) 0 0))
        ((R) (point (cdr d) 0 0)))))

  (let loop ((w w) (pos *origin-point*) (ps (list)))
    (if (null? w)
        (flatten (reverse ps))
        (let* ((npos (move pos (car w)))
               (pts (line->points (line pos npos))))
          (loop (cdr w)
                npos
                (cons (cdr pts) ps))))))

(define (best-intersection ws f)
  (let* ((e0 (list->set (first ws)))
         (e1 (list->set (second ws)))
         (e (set-intersect e0 e1)))
    (argmin f (set->list e))))

(define solve-a
  (let ((pd (curry point-manhattan *origin-point*)))
    (compose pd (curryr best-intersection pd))))

(define (steps-to ws p)
  (sum (map (compose add1 (curryr index-of p)) ws)))

(define (solve-b ws)
  (steps-to ws (best-intersection ws (curry steps-to ws))))

(define today (list parse (curry map sim) solve-a solve-b (const #t)))
