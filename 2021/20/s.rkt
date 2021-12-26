#lang racket

(require "../../advent.rkt")
(require srfi/60)

; Day 20: Trench Map

(define cpoint? (cons/c integer? integer?))
(define (cpoint+ a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define transform? (vectorof boolean?))
(define image? (cons/c boolean? (hash/c cpoint? boolean?)))

(define/contract (parse ls)
  (-> (listof string?) (cons/c transform? image?))

  (define/contract (build-imap ls)
    (-> (listof string?) (hash/c cpoint? boolean?))
    (let yloop ((y 0) (ls ls) (s (hash)))
      (if (null? ls)
          s
          (let xloop ((x 0) (r (car ls)) (s s))
            (if (= x (string-length r))
                (yloop (+ y 1) (cdr ls) s)
                (xloop (+ x 1) r
                      (hash-set s (cons x y)
                                (char=? (string-ref r x) #\#))))))))

  (cons
    (list->vector (map (curry char=? #\#) (string->list (first ls))))
    (cons #f (build-imap (drop ls 2)))))

(define (cell-index s p)
  ; Note: checking this contract imposes like a 1000x runtime hit
  ; dynamic typing is based
  ;(-> image? cpoint? integer?)

  (define (m x y)
    (hash-ref (cdr s) (cons (+ (car p) x) (+ (cdr p) y)) (car s)))

  (list->integer
    (list (m -1 -1) (m 0 -1) (m 1 -1)
          (m -1 0) (m 0 0) (m 1 0)
          (m -1 1) (m 0 1) (m 1 1))))

(define/contract (enhance t s)
  (-> transform? image? image?)
  (define/contract (npx p)
    (-> cpoint? boolean?)
    (vector-ref t (cell-index s p)))

  (let ((default (car s)) (pixels (cdr s)))
    (let ((minx (sub1 (apply min (map car (hash-keys pixels)))))
          (miny (sub1 (apply min (map cdr (hash-keys pixels)))))
          (maxx (add1 (apply max (map car (hash-keys pixels)))))
          (maxy (add1 (apply max (map cdr (hash-keys pixels)))))
          (ns (make-hash)))
      (for ([y (add1 (- maxy miny))])
        (for ([x (add1 (- maxx minx))])
          (let ((p (cons (+ x minx) (+ y miny))))
            (hash-set! ns p (npx p)))))
     (cons
       (vector-ref t (if default 511 0))
       ns))))

(define (lit-pixels i)
  (if (car i)
      -1
      (count identity (hash-values (cdr i)))))

(define (solve-part n p)
  (lit-pixels (iterate-n (curry enhance (car p)) n (cdr p))))

(define solve
  (fork
    (curry solve-part 2)
    (curry solve-part 50)))

(solve! 20 parse solve)
