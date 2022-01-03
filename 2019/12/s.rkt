#lang racket

(provide today)
(require "../../lib/func.rkt"
         "../../lib/geom.rkt"
         "../../lib/list.rkt")

(struct moon (pos vel) #:transparent)

(define/contract parse-moon
  (-> string? moon?)
  (compose (curryr moon *origin-point*)
           string->point
           (curryr string-replace "<x=" "")
           (curryr string-replace " y=" "")
           (curryr string-replace " z=" "")
           (curryr string-replace ">"   "")))

(define (apply-gravity ms)
  (define (perturb p ps)
    (let loop ((v 0) (ps ps))
      (cond
        ((null? ps)     v)
        ((> (car ps) p) (loop (add1 v) (cdr ps)))
        ((< (car ps) p) (loop (sub1 v) (cdr ps)))
        (else           (loop v (cdr ps))))))

  (define (apply-gravity-from xs ys zs m)
    (let ((dvx (perturb (point-x (moon-pos m)) xs))
          (dvy (perturb (point-y (moon-pos m)) ys))
          (dvz (perturb (point-z (moon-pos m)) zs)))
      (struct-copy moon m (vel (point+ (moon-vel m) (point dvx dvy dvz))))))

  (let* ((ps (map moon-pos ms))
         (xs (sort (map point-x ps) <))
         (ys (sort (map point-y ps) <))
         (zs (sort (map point-z ps) <)))
    (map (curry apply-gravity-from xs ys zs) ms)))

(define (apply-velocity ms)
  (map
    (lambda (m) (struct-copy moon m (pos (point+ (moon-pos m) (moon-vel m)))))
    ms))

(define (step ms)
  (apply-velocity (apply-gravity ms)))

(define (energy ms)
  (define (asum p)
    (+ (abs (point-x p))
       (abs (point-y p))
       (abs (point-z p))))

  (define (potential-of m) (asum (moon-pos m)))
  (define (kinetic-of m) (asum (moon-vel m)))

  (define (energy-of m)
    (* (potential-of m) (kinetic-of m)))

  (sum (map energy-of ms)))

(define (solve-a ms)
  (energy (iterate-n step 1000 ms))) 

(define (findrec ms)
  ; Key insight here: each dimension is independent in the simulation; the x
  ; position and velocity of a moon are only affected by the x positions of
  ; other moons. We can treat the system as being made up of three independent
  ; systems, find how long it takes each of those systems to recur, then
  ; solve the system of equations to find out when the recurrence for the main
  ; system would be.

  (define (grav-for pvs p)
    (foldl
      (lambda (pv d)
        (cond
          ((> (car pv) p) (add1 d))
          ((< (car pv) p) (sub1 d))
          (else           d)))
      0 pvs))

  (define (apply-grav-d pvs)
    (map
      (lambda (pv) (cons (car pv)
                   (+ (cdr pv) (grav-for pvs (car pv)))))
      pvs))

  (define (apply-vels-d pvs)
    (map
      (lambda (pv) (cons (+ (car pv) (cdr pv)) (cdr pv)))
      pvs))

  (define (step-d pvs)
    (apply-vels-d (apply-grav-d pvs)))

  (define (findrec-d pvs)
    (let loop ((pvs pvs) (vis (set)) (n 0))
      (let ((npvs (step-d pvs)))
        (if (set-member? vis npvs)
            (values npvs n)
            (loop npvs (set-add vis npvs) (add1 n))))))

  (define (make-pvs ms)
    (let ((ps (map moon-pos ms))
          (zc (lambda (c) (cons c 0))))
      (values (map (compose zc point-x) ps)
              (map (compose zc point-y) ps)
              (map (compose zc point-z) ps))))

  (let-values (((xp yp zp) (make-pvs ms)))
    (let-values (((xr xn) (findrec-d xp))
                 ((yr yn) (findrec-d yp))
                 ((zr zn) (findrec-d zp)))

      ; Important safety tip: if the recurrent state wasn't the initial state
      ; here, we'd need to solve a system of equations to figure out which one
      ; it is. Fortunately, on my input, it appears to be.
      (lcm xn yn zn))))

(define today (list (curry map parse-moon)
                    identity
                    solve-a
                    findrec))
