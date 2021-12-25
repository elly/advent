#lang racket

(require "../advent.scm")

; Day 11: Dumbo Octopus
; We have an nxn grid of octopuses, each with an integer energy level.
; Each step:
; - Each octopus's energy level goes up by 1
; - Any octopus with >9 energy flashes: each octopus adjacent to it gains 1
;   energy and may also flash. Each octopus can flash at most once per step.
; - Any octopus that flashed has its energy set to 0

(define/contract parse
  (-> (listof string?) vec2?)
  (compose list->vector
           (curry map
              (compose list->vector
                       (curry map (compose string->number string))
                       string->list))))

(define/contract raise-energy
  (-> vec2? vec2?)
  (curry vec2-map add1))

; Given a vec2 of octopus energy levels and a set of which octopi have already
; flashed, return a set of octopi that are about to flash.
(define/contract (about-to-flash v f)
  (-> vec2? (set/c point2?) (set/c point2?))
  (list->set
    (filter
      (lambda (p)
        (and (> (vec2-at v p) 9)
             (not (set-member? f p))))
      (vec2-indexes v))))

(define/contract (flash v s)
  (-> vec2? (set/c point2?) vec2?)
  (foldl
    (lambda (p v)
       (let ((ns (list->set (vec2-neighbors-d v p))))
         (vec2-mapi
           (lambda (np) (if (set-member? ns np)
                            (add1 (vec2-at v np))
                            (vec2-at v np)))
           v)))
    v (set->list s)))

(define/contract (zero-flashed v fs)
  (-> vec2? (set/c point2?) vec2?)
  (vec2-mapi
    (lambda (p) (if (set-member? fs p)
                    0
                    (vec2-at v p)))
    v))

(define/contract (flash-all v)
  (-> vec2? (values vec2? integer?))
  (let loop ((v v) (fs (set)))
    (let ((nf (about-to-flash v fs)))
      (if (set-empty? nf)
          (values (zero-flashed v fs) (set-count fs))
          (loop (flash v nf) (set-union fs nf))))))

(define/contract (step v)
  (-> vec2? (values vec2? integer?))
  (let-values (((v n) (flash-all (raise-energy v))))
    (values v n)))

(define/contract (stepn v n)
  (-> vec2? integer? integer?)
  (let loop ((v v) (n n) (t 0))
    (if (= 0 n)
        t
        (let-values (((nv d) (step v)))
          (loop nv (sub1 n) (+ t d))))))

(define/contract (step-until-all v)
  (-> vec2? integer?)
  (let loop ((v v) (n 1))
    (let-values (((nv t) (step v)))
      (if (= t (* (vector-length nv) (vector-length (vector-ref nv 0))))
          n
          (loop nv (add1 n))))))

(define solve
  (fork
    (curryr stepn 100)
    (curry step-until-all)))

(solve! 11 parse solve)
