#lang racket

(provide point point? point-x point-y point-z)
(provide line line? line-start line-end)

(provide string->point unitize-point point+ point-)
(provide line->points line-cardinal?)

(struct point (x y z) #:transparent)
(struct line (start end) #:transparent)

(define (string->point s)
  (let ((ps (string-split s ",")))
    (point (string->number (first ps))
           (string->number (second ps))
           (if (>= (length ps) 3)
               (string->number (third ps))
               0))))

(define (unitize d)
  (if (= d 0) 0 (/ d (abs d))))

(define (unitize-point p)
  (point (unitize (point-x p))
         (unitize (point-y p))
         (unitize (point-z p))))

(define/contract (line->points l)
  (-> line? (listof point?))
  (let* ((s (line-start l))
         (e (line-end l))
         (d (unitize-point (point- e s))))
    (let loop ((r '()) (p s))
      (if (equal? p e)
        (reverse (cons p r))
        (loop (cons p r) (point+ p d))))))

(define (point+ a b)
  (point (+ (point-x a) (point-x b))
         (+ (point-y a) (point-y b))
         (+ (point-z a) (point-z b))))

(define (point- a b)
  (point (- (point-x a) (point-x b))
         (- (point-y a) (point-y b))
         (- (point-z a) (point-z b))))

(define (line-cardinal? l)
  (let ((s (line-start l)) (e (line-end l)))
    (let ((sx (point-x s)) (sy (point-y s)) (sz (point-z s))
          (ex (point-x e)) (ey (point-y e)) (ez (point-z e)))
      (or (and (= sx ex) (= sy ey))
          (and (= sx ex) (= sz ez))
          (and (= sy ey) (= sz ez))))))
