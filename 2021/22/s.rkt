#lang racket

(require "../../advent.scm")

; Day 22: Reactor Reboot
;
; We are given a list of instructions, each of which specifies a state and
; a trio of inclusive ranges, like:
;   on x=10..12,y=10..12,z=10..12
; which enables 27 cubes.

(define range? (cons/c integer? integer?))
(define cuboid? (list/c range? range? range?))
(define instruction? (cons/c boolean? cuboid?))
(define world? (set/c cuboid?))

(define/contract (parse ls)
  (-> (listof string?) (listof instruction?))

  (define/contract (parse-range r)
    (-> string? range?)
    (let* ((rp (string-split (second (string-split r "=")) "..")))
      (cons (s->i (first rp)) (s->i (second rp)))))

  (map
    (lambda (li)
      (let ((sp (string-split li " ")))
        (cons (string=? (first sp) "on")
              (map parse-range (string-split (second sp) ",")))))
    ls))

(define/contract (intersect c0 c1)
  (-> cuboid? cuboid? (or/c cuboid? #f))

  (define/contract (intersect-r r0 r1)
    (-> range? range? (or/c range? #f))
    (let ((r0i (car r0)) (r0x (cdr r0)) (r1i (car r1)) (r1x (cdr r1)))
      (if (or (and (>= r0i r1i) (<= r0i r1x))
              (and (>= r1i r0i) (<= r1i r0x)))
          (cons (max r0i r1i) (min r0x r1x))
          #f)))

  (let ((xr (intersect-r (first c0) (first c1)))
        (yr (intersect-r (second c0) (second c1)))
        (zr (intersect-r (third c0) (third c1))))
    (if (and xr yr zr)
        (list xr yr zr)
        #f)))

(define/contract (overlapping? c0 c1)
  (-> cuboid? cuboid? boolean?)
  (not (not (intersect c0 c1))))

(define (xmin c) (car (first c)))
(define (xmax c) (cdr (first c)))
(define (ymin c) (car (second c)))
(define (ymax c) (cdr (second c)))
(define (zmin c) (car (third c)))
(define (zmax c) (cdr (third c)))

(define/contract (clip c a r)
  (-> cuboid? (or/c 'x 'y 'z) range? (or/c cuboid? #f))
  (intersect c
     (list (if (eq? a 'x) r (first c))
           (if (eq? a 'y) r (second c))
           (if (eq? a 'z) r (third c)))))

; Given a cuboid c and a "dividend" cuboid d, generate a set of cuboids cs ;
; occupying the points that are in c but not d - i.e., with the property that,
; for every element e of cs:
;   (not (overlapping? e d))
;
; The silliest way to do this, but the one that avoids special cases, is to
; fragment c into up to 6 non-overlapping pieces on the various sides of the
; overlap between c and d.
(define/contract (fragment c d)
  (-> cuboid? cuboid? (set/c cuboid?))

  (define *infty* 99999999)
  (define *-infty* -99999999)

  (let* ((i (intersect c d))
         (zt (clip c 'z (cons (add1 (zmax i)) *infty*)))
         (zb (clip c 'z (cons *-infty* (sub1 (zmin i)))))
         (c (clip c 'z (third i)))
         (yt (clip c 'y (cons (add1 (ymax i)) *infty*)))
         (yb (clip c 'y (cons *-infty* (sub1 (ymin i)))))
         (c (clip c 'y (second i)))
         (xt (clip c 'x (cons (add1 (xmax i)) *infty*)))
         (xb (clip c 'x (cons *-infty* (sub1 (xmin i))))))
    (list->set (filter identity (list zt zb yt yb xt xb)))))

; Given an instruction and a world (a set of lit cuboids), produce a new world.
; It is both a precondition and a postcondition of this function that none of
; the cuboids in the world overlap.
(define/contract (step i w)
  (-> instruction? world? world?)

  ; To do this, we iterate over all the existing cuboids in the world. For each
  ; cuboid e in the world, if e doesn't overlap with i, we leave it untouched.
  ; If it does, we fragment e, remove e, and insert all the fragments of e.
  ; Once we're done with that, if i is an on instruction, we insert i.
  (define/contract (step-c ic c)
    (-> cuboid? cuboid? (set/c cuboid?))
    (if (not (overlapping? ic c))
        (set c)
        (fragment c ic)))

  (let* ((fs (set-map w (curry step-c (cdr i))))
         (r (foldl set-union (set) fs)))
    (if (car i)
        (set-add r (cdr i))
        r)))

(define/contract (sum-volumes w)
  (-> world? integer?)
  (foldl
    (lambda (v s)
      (+ s (* (add1 (- (xmax v) (xmin v)))
              (add1 (- (ymax v) (ymin v)))
              (add1 (- (zmax v) (zmin v))))))
    0
    (set->list w)))

(define *part-a-box* '((-50 . 50) (-50 . 50) (-50 . 50)))

(define solve
  (fork
    (compose sum-volumes
             list->set
             (curry filter-map (curry intersect *part-a-box*))
             set->list
             (curry foldl step (set)))
    (compose sum-volumes
             (curry foldl step (set)))))

(solve! 22 parse solve)
