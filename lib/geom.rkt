#lang racket

(require quaternion)

(provide point point? point-x point-y point-z)
(provide line line? line-start line-end)

(provide string->point unitize-point point+ point- point-manhattan)
(provide line->points line-cardinal?)
(provide point-neighbors points-coplanar-in?)
(provide *origin-point*)

(provide rect rect? rect-contains? rect-min rect-max)

(provide rotations-of)

(define plane? (or/c 'x 'y 'z))

(struct point (x y z) #:transparent)
(struct line (start end) #:transparent)
(struct rect (min max) #:transparent)

(define *origin-point* (point 0 0 0))

(define (string->point s)
  (let ((ps (string-split s ",")))
    (and (>= (length ps) 2)
         (point (string->number (first ps))
                (string->number (second ps))
                (if (>= (length ps) 3)
                    (string->number (third ps))
                    0)))))

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

(define (point-manhattan a b)
  (+ (abs (- (point-x a) (point-x b)))
     (abs (- (point-y a) (point-y b)))
     (abs (- (point-z a) (point-z b)))))

(define (line-cardinal? l)
  (let ((s (line-start l)) (e (line-end l)))
    (let ((sx (point-x s)) (sy (point-y s)) (sz (point-z s))
          (ex (point-x e)) (ey (point-y e)) (ez (point-z e)))
      (or (and (= sx ex) (= sy ey))
          (and (= sx ex) (= sz ez))
          (and (= sy ey) (= sz ez))))))

(define (point-neighbors p)
  (let ((ds (remove '(0 0 0)
                     (cartesian-product '(-1 0 1) '(-1 0 1) '(-1 0 1)))))
    (map (compose (curry point+ p) (curry apply point)) ds)))

(define (points-coplanar-in? plane a b)
  (case plane
    [(x) (= (point-x a) (point-x b))]
    [(y) (= (point-y a) (point-y b))]
    [(z) (= (point-z a) (point-z b))]
    [else (error "plane ~a" plane)]))

(define (rect-contains? r p)
  (and (>= (point-x p) (point-x (rect-min r)))
       (<= (point-x p) (point-x (rect-max r)))
       (>= (point-y p) (point-y (rect-min r)))
       (<= (point-y p) (point-y (rect-max r)))
       (>= (point-z p) (point-z (rect-min r)))
       (<= (point-z p) (point-z (rect-max r)))))

; A rotation of angle theta around a unit vector axis #(ux uy uz) is the
; quaternion (cos (/ theta 2)) + #(ux uy uz) (sin (/ theta 2)) where the #(ux uy
; uz) are notionally multipliers for the three cartesian axis unit vectors
; #(1 0 0) #(0 1 0) #(0 0 1).
;
; For this problem, theta is always either 0, (/ pi 2), pi, or (/ (* 3 pi) 2),
; and so the cosine terms are:
;   1    0   -1   0
; and the sine terms are:
;   0    1   0   -1
;
; So for a given unit vector axis, the four rotations are:
;   1 + #(ux uy uz) 0
;   0 + #(ux uy uz) 1
;  -1 + #(ux uy uz) 0
;   0 + #(ux uy uz) -1
;
; So we have #(1 0 0 0), #(0 ux uy uz), #(-1 0 0 0), and #(0 -ux -uy -uz)
; as the four rotations. We can multiply the actual vector we want to rotate
; by each of these four in turn, once for each of the six cartesian unit
; vectors, to produce the 24 rotations of the vector, although there may be
; duplicates.

(define/contract (rotations-about qv)
  (-> qvector? (listof quaternion?))
  (list
    (q-rotation 0.0 qv)
    (q-rotation (/ pi 2) qv)
    (q-rotation pi qv)
    (q-rotation (/ (* 3 pi) 2) qv)))

(define qid (q-rotation 0.0      (qvector 1.0 0.0 0.0)))
(define qrx (q-rotation (/ pi 2) (qvector 1.0 0.0 0.0)))
(define qry (q-rotation (/ pi 2) (qvector 0.0 1.0 0.0)))
(define qrz (q-rotation (/ pi 2) (qvector 0.0 0.0 1.0)))

(define/contract (z-rotations-of q)
  (-> quaternion? (listof quaternion?))
  (let ((qp q-multiply-qq))
    (list q (qp q qrz) (qp q (qp qrz qrz)) (qp q (qp qrz (qp qrz qrz))))))

(define/contract rotations
  (vectorof quaternion?)
  (let ((qp q-multiply-qq))
    (list->vector
      (append
         ; let's view all the other rotations as being a combination of x & y
         ; rotations that select a top face, then some number of z rotations
         ; that orient it. First, for the starting face:
         (z-rotations-of qid)

         ; then 1, 2, or 3 rotations about the x axis:
         (z-rotations-of qrx)
         (z-rotations-of (qp qrx qrx))
         (z-rotations-of (qp qrx (qp qrx qrx)))

         ; then 1 or 3 rotations about the y axis - 2 would produce overlap
         ; with the 2 case above:
         (z-rotations-of qry)
         (z-rotations-of (qp qry (qp qry qry)))))))
        

(define/contract (qvector->point v)
  (-> qvector? point?)
  (point (exact-round (qvector-x v))
         (exact-round (qvector-y v))
         (exact-round (qvector-z v))))

(define/contract (point->qvector v)
  (-> point? qvector?)
  (qvector (exact->inexact (point-x v))
           (exact->inexact (point-y v))
           (exact->inexact (point-z v))))

(define/contract (rotations-of v)
  (-> point? (listof point?))
  (map (compose qvector->point
                quaternion-v
                (curryr q-rotate (quaternion 0.0 (point->qvector v))))
       (vector->list rotations)))
