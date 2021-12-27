#lang racket

(provide today)
(require "../../lib/set.rkt")

; Day 12: Passage Pathing
; We are given a list of adjacent node pairs that form a graph. Some of the
; nodes (those written in uppercase) are 'big' and we can re-traverse them;
; others are 'small' and we can visit them at most once. We want to find
; the set of all paths through the graph under that constraint.
;
; We could do that by keeping a visited set, but instead we do it by contracting
; the graph whenever we've visited a node - ie, if we've visited a small node
; already, we delete all its edges from the graph.
(define node? symbol?)
(define edge? (cons/c node? node?))
(define graph? (listof edge?))
(define path? (listof node?))

(define/contract parse
  (-> (listof string?) graph?)
  (curry map
        (lambda (s)
          (let ((p (map string->symbol (string-split s "-"))))
            (cons (first p) (second p))))))

(define small? (compose char-lower-case?
                        (curryr string-ref 0)
                        symbol->string))

(define terminal? (or/c (curry symbol=? 'start) (curry symbol=? 'end)))

(define/contract (neighbors g n)
  (-> graph? node? (set/c node?))
  (list->set
    (filter-map (lambda (p)
      (cond
        [(symbol=? (car p) n) (cdr p)]
        [(symbol=? (cdr p) n) (car p)]
        [else #f]))
      g)))

(define/contract (remove-node g n)
  (-> graph? node? graph?)
  (filter-not
    (lambda (e) (or (symbol=? (car e) n) (symbol=? (cdr e) n)))
    g))

(define/contract (remove-small g n)
  (-> graph? node? graph?)
  (if (small? n) (remove-node g n) g))

; Given a graph and two nodes s and e, produce a set of all paths between
; s and e.
(define/contract (findpaths g s e vs)
  (-> graph? node? node? boolean? (set/c path?))

  (define (fpn g e s vs n)
    (list->set
      (set-map (findpaths g n e vs)
               (curry cons s))))

  (if (symbol=? s e)
      (set (list e))
      (let ((ns (neighbors g s))
            (ng (remove-small g s)))
        (if (set-empty? ns)
            (set)
            (let ((mp0 (set-map ns (curry fpn ng e s vs)))
                  (mp1 (if (and (and (not vs) (small? s))
                                (not (terminal? s)))
                           (set-map ns (curry fpn g e s #t))
                           (list (set)))))
              (set-union (apply set-union mp0)
                         (apply set-union mp1)))))))

(define extract (curryr findpaths 'start 'end #f))

(define (no-repeats? p)
  (not (check-duplicates p
                         (lambda (a b)
                                 (and (small? a) (small? b) (equal? a b)))
                         #:default #f)))

(define solve-a
  (compose set-count (curry set-filter no-repeats?)))
(define solve-b set-count)

(define today (list parse extract solve-a solve-b (const #t)))
