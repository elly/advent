; Advent library functions that I end up reusing a lot
#lang racket

(require quaternion)

(provide solve!)
(provide s->i s->ca s->y)
(provide sum prod fork project)
(provide group)
(provide iterate-n)

(provide point2? point3? point+ point- unitize)
(provide make-rect rect? rect-bottom rect-contains?)
(provide list->vec2 vec2? vec2-inbounds? vec2-neighbors vec2-neighbors-d
         vec2-at vec2-indexes vec2-mapi vec2-map vec2-values
         vec2-bottom-right)

(provide rotations-of nth-rotation-of)

(define s->i string->number)
(define s->ca (compose list->vector string->list))
(define s->y string->symbol)

(define (read-all-lines p)
  (let loop ((ls '()))
    (let ((l (read-line p)))
      (if (eof-object? l)
        (reverse ls)
        (loop (cons l ls))))))

; solve takes a day number and two:
; a parse function, which accepts a list of strings and returns an A
; a solve function, which accepts an A and returns a pair of strings-or-ints
(define (solve! n pf sf)
  (let* ((f (open-input-file (vector-ref (current-command-line-arguments) 0)))
         (lines (read-all-lines f))
         (pr (pf lines))
         (sr (sf pr)))
    (printf "~a~n~a~n" (car sr) (cdr sr))))

(define sum (curry foldl + 0))
(define prod (curry foldl * 1))

(define (fork f g)
  (lambda args
    (cons (apply f args) (apply g args))))

(define/contract (project l . fs)
  (-> (listof any/c) procedure? ... (listof any/c))
  (map (lambda (f e) (f e)) fs l))

(define point2? (list/c integer? integer?))
(define point3? (list/c integer? integer? integer?))

(define/contract (point- p0 p1)
  (-> (listof integer?) (listof integer?) (listof integer?))
  (map - p0 p1))

(define/contract (point+ p0 p1)
  (-> (listof integer?) (listof integer?) (listof integer?))
  (map + p0 p1))

(define unitize
  (curry map
    (lambda (e) (if (= e 0) 0 (/ e (abs e))))))

(define (make-rect p0 p1)
  (let ((bl (list (min (first p0) (first p1))
                  (min (second p0) (second p1))))
        (tr (list (max (first p0) (first p1))
                  (max (second p0) (second p1)))))
    (list bl tr)))

(define rect? (list/c point2? point2?))
(define rect-bottom (compose second first))
(define (rect-contains? r p)
  (let ((bl (first r)) (tr (second r)) (x (first p)) (y (second p)))
    (and (>= x (first bl)) (>= y (second bl))
         (<= x (first tr)) (<= y (second tr)))))

(define (list->vec2 ls)
  (build-vector (length ls)
    (lambda (i)
      (list->vector (list-ref ls i)))))

(define (vec2? v)
  (and (vector? v)
       (andmap vector? (vector->list v))))

(define/contract (vec2-inbounds? v p)
  (-> vec2? point2? boolean?)
  (and (>= (first p) 0)
       (< (first p) (vector-length (vector-ref v 0)))
       (>= (second p) 0)
       (< (second p) (vector-length v))))

(define (vec2-neighbors v p)
  (filter (curry vec2-inbounds? v)
    (list (point+ p '(0 1)) (point+ p '(0 -1))
          (point+ p '(1 0)) (point+ p '(-1 0)))))

(define (vec2-neighbors-d v p)
  (filter (curry vec2-inbounds? v)
    (list (point+ p '(-1 -1)) (point+ p '(0 -1)) (point+ p '(1 -1))
          (point+ p '(-1  0))                    (point+ p '(1  0))
          (point+ p '(-1  1)) (point+ p '(0  1)) (point+ p '(1  1)))))

(define (vec2-at v p)
  (vector-ref (vector-ref v (second p)) (first p)))

(define/contract (vec2-indexes v)
  (-> vec2? (listof point2?))
  (cartesian-product
    (build-list (vector-length (vector-ref v 0)) identity)
    (build-list (vector-length v) identity)))

(define/contract (vec2-mapi p v)
  (-> procedure? vec2? vec2?)
  (build-vector (vector-length v)
    (lambda (y)
       (build-vector (vector-length (vector-ref v y))
                     (lambda (x) (p (list x y)))))))

(define/contract (vec2-map p v)
  (-> procedure? vec2? vec2?)
  (vec2-mapi (compose p (curry vec2-at v)) v))

(define/contract vec2-values
  (-> vec2? (listof any/c))
  (compose (curry apply append) vector->list (curry vector-map vector->list)))

(define/contract (vec2-bottom-right v)
  (-> vec2? point2?)
  (list (- (vector-length (vector-ref v 0)) 1)
        (- (vector-length v) 1)))

(define/contract (group sp lst)
  (-> procedure? (listof any/c) (listof (listof any/c)))
  (let loop ((r '()) (lst lst))
    (cond
      [(null? lst) (if r (list (reverse r)) '())]
      [(sp (car lst)) (cons (reverse r) (group sp (cdr lst)))]
      [else (loop (cons (car lst) r) (cdr lst))])))

(define/contract (iterate-n f n x)
  (-> procedure? integer? any/c any/c)
  (let loop ((n n) (x x))
    (if (= n 0)
      x
      (loop (- n 1) (f x)))))

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
        

(define/contract (qvector->point3 v)
  (-> qvector? point3?)
  (map exact-round (list (qvector-x v) (qvector-y v) (qvector-z v))))

(define/contract (point3->qvector v)
  (-> point3? qvector?)
  (let ((v (map exact->inexact v)))
    (qvector (first v) (second v) (third v))))

(define/contract (rotations-of v)
  (-> point3? (listof point3?))
  (map (compose qvector->point3
                quaternion-v
                (curryr q-rotate (quaternion 0.0 (point3->qvector v))))
       (vector->list rotations)))

(define/contract (nth-rotation-of n v)
  (-> integer? point3? point3?)
  (qvector->point3
    (quaternion-v
      (q-rotate (vector-ref rotations n)
                (quaternion 0.0 (point3->qvector v))))))

