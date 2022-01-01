#lang racket

(provide today)
(require "../../lib/geom.rkt"
         "../../lib/list.rkt"
         "../../lib/vec.rkt")

(define parse (curry line-list->vec2 (curry char=? #\#)))

(define (points-on p0 p1)
  (define (gen-points p0 p1 d)
    (let loop ((ps (list)) (p0 p0))
      (if (equal? p0 p1)
          (reverse (cons p0 ps))
          (loop (cons p0 ps) (point+ p0 d)))))

  (let ((dx (- (point-x p1) (point-x p0)))
        (dy (- (point-y p1) (point-y p0))))
    (if (or (= dx 0) (= dy 0))
        (gen-points p0 p1 (unitize-point (point dx dy 0)))
        (let ((e (gcd dx dy)))
          (gen-points p0 p1 (point (/ dx e) (/ dy e) 0))))))

(define (los? m p0 p1)
  (let ((ps (drop (drop-right (points-on p0 p1) 1) 1)))
    (andmap (negate (curry vec2-at m)) ps)))

(define (losfrom m p0)
  (cons p0
    (if (not (vec2-at m p0))
        0
        (sum
          (vec2-values
            (vec2-mapi
              (lambda (p1)
                (if (and (not (equal? p0 p1))
                         (vec2-at m p1)
                         (los? m p0 p1))
                    1 0))
              m))))))

(define (maxlos m)
  (let ((lm (vec2-mapi (curry losfrom m) m)))
    (first (sort (vec2-values lm) > #:key cdr))))

(define (zap m p0)
  (define (seen-asteroids)
    (filter identity
      (vec2-values
        (vec2-mapi
          (lambda (p1) (and (not (equal? p0 p1))
                            (vec2-at m p1)
                            (los? m p0 p1)
                            p1))
          m))))

  ; This is extremely gross: we compute a fake angle from p0 to p1, using a very
  ; large offset to map the four quadrants into the same space. Note that
  ; sometimes we divide ady by adx and sometimes adx by ady to get the desired
  ; ordering :(
  (define (fake-angle-from-p0 p1)
    (define *offset-hack* 100000)
    (let* ((dy (- (point-y p1) (point-y p0)))
           (dx (- (point-x p1) (point-x p0)))
           (ady (abs dy))
           (adx (abs dx))
           (ae (gcd ady adx)))
      (cond
        ((and (< dy 0) (= dx 0)) 0)
        ((and (< dy 0) (> dx 0)) (/ adx ady))
        ((and (= dy 0) (> dx 0)) *offset-hack*)
        ((and (> dy 0) (> dx 0)) (+ *offset-hack*) (/ ady adx))
        ((and (> dy 0) (= dx 0)) (* 2 *offset-hack*))
        ((and (> dy 0) (< dx 0)) (+ (* 2 *offset-hack*) (/ adx ady)))
        ((and (= dy 0) (< dx 0)) (* 3 *offset-hack*))
        ((and (< dy 0) (< dx 0)) (+ (* 3 *offset-hack*) (/ ady adx))))))

  (sort (seen-asteroids) < #:key fake-angle-from-p0))

(define (locate m)
  (let ((r (maxlos m)))
    (list m (car r) (cdr r))))

(define (solve-b s)
  (define (remove-zapped m zps)
    (vec2-mapi
      (lambda (p) (and (vec2-at m p) (not (member p zps))))
      m))

  (define (do-zaps)
    (let ((p (second s)))
      (let loop ((m (first s)) (zc 200))
        (let ((zps (zap m p)))
          (if (>= (length zps) zc)
              (list-ref zps (sub1 zc))
              (loop (remove-zapped m zps) (- zc (length zps))))))))

  (if (> (length (vec2-values (first s))) 200)
      (let ((r (do-zaps)))
        (+ (* (point-x r) 100) (point-y r)))
      0))

(define today (list parse locate third solve-b))
