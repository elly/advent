#lang racket

(require "../../advent.scm")

; Day 19: Beacon Scanner
;
; We are given a series of scan results, one per scanner. Scanners detect all
; the beacons within a 2000-unit cube centered on the scanner (ie, within 1000
; units in all directions). However, we don't know the positions of the scanners
; or their orientation/facing; there are 24 possible orientations they could
; be in, since they can face positive/negative x/y/z and consider any of the
; four directions "up" from there.
;
; So, we can represent our input as a list of scanner results, each of which
; is a set of integer vectors.

(define beacon? (vectorof (listof integer?)))
(struct scan-result (orientation location beacons) #:transparent)

(define (update-orientation sr n)
  (struct-copy scan-result sr (orientation n)))
(define (update-location sr v)
  (struct-copy scan-result sr (location v)))

(define oriented? scan-result-orientation)
(define located? scan-result-location)

(define oriented-scan-result? (and/c scan-result? oriented?))
(define located-scan-result? (and/c scan-result? oriented? located?))

(define/contract (scan-result-plane s n)
  (-> scan-result? integer? (set/c point3?))
  (list->set (map (curryr vector-ref n)
                  (set->list (scan-result-beacons s)))))

(define/contract (oriented-points s)
  (-> oriented-scan-result? (set/c point3?))
  (scan-result-plane s (scan-result-orientation s)))

(define/contract (canonical-points s)
  (-> located-scan-result? (set/c point3?))
  (list->set
     (map (curryr point+ (scan-result-location s))
       (set->list (scan-result-plane s (scan-result-orientation s))))))

(define/contract (parse ls)
  (-> (listof string?) (listof scan-result?))
  
  (define/contract (split-results ls)
    (-> (listof string?) (listof (listof string?)))
    (if (null? ls)
        '()
        (let-values ([(h t) (splitf-at ls non-empty-string?)])
          (cons h (split-results (if (null? t) t (cdr t)))))))

  (define/contract parse-result
    (-> (listof string?) scan-result?)
    (compose (curry scan-result #f #f)
             list->set
             (curry map (compose list->vector
                                 rotations-of
                                 (curry map s->i)
                                 (curryr string-split ",")))
             cdr))
  (map parse-result (split-results ls)))

(define/contract (make-delta-set ps)
  (-> (set/c point3?) (values (set/c point3?) hash?))
  (let ((lps (set->list ps)))
    (let loop ((s (set)) (m (hash))
               (pc (append (combinations lps 2)
                           (combinations (reverse lps) 2))))
      (if (null? pc)
          (values s m)
          (let ((d (point- (second (car pc)) (first (car pc)))))
            (loop (set-add s d) (hash-set m d (car pc)) (cdr pc)))))))

(define/contract (points-for-delta-set m ps)
  (-> hash? (set/c point3?) (set/c point3?))
  (let ((pps (map (curry hash-ref m) (set->list ps))))
    (list->set (apply append pps))))

; This function tries to match up two scan results a and b, each of which may
; be rotated and offset relative to each other. To do this, it fixes the
; orientation of a, then for every orientation of b, tries finding at least 12
; overlapping points with a, and if it does, returns the new orientation to use
; for b.

(define/contract (match-scan-result-rotation a b)
  (-> oriented-scan-result? scan-result? (or/c oriented-scan-result? #f))

  (define/contract (rotation-matches? ad am b rn)
    (-> (set/c point3?) hash? scan-result? integer? boolean?)
    (let-values ([(bd bm) (make-delta-set (scan-result-plane b rn))])
      (let ((r (set-count (points-for-delta-set am (set-intersect ad bd)))))
        (>= r (min 12 (set-count (scan-result-plane b rn)))))))

  (let ((ap (oriented-points a)))
    (let-values ([(ad am) (make-delta-set ap)])
    (let loop ((rn 0))
      (if (= rn 24)
          #f
          (if (rotation-matches? ad am b rn)
              (update-orientation b rn)
              (loop (add1 rn))))))))

(define/contract (orient-some srs)
  (-> (listof scan-result?) (listof scan-result?))
  ; (printf "os ~a~n" (map scan-result-orientation srs))
  (map
    (lambda (mu)
      (if (oriented? mu)
          mu
          (or (ormap (curryr match-scan-result-rotation mu)
                     (filter oriented? srs))
              mu)))
    srs))

(define/contract (orient-first srs)
  (-> (listof scan-result?) (listof scan-result?))
  (cons (update-orientation (car srs) 0) (cdr srs)))

(define/contract (orient-all srs)
  (-> (listof scan-result?) (listof oriented-scan-result?))
  (let loop ((srs (orient-first srs)))
    (if (andmap oriented? srs)
        srs
        (loop (orient-some srs)))))

(define/contract (match-scan-result-location a b)
  (-> located-scan-result? oriented-scan-result?
      (or/c located-scan-result? #f))

  (define/contract (matching-points ap bp v)
    (-> (set/c point3?) (set/c point3?) point3? integer?)
    (let ((bps (list->set (map (curry point+ v) (set->list bp)))))
      ; (printf "mp ~a ~a ~a -> ~a~n" (set-count ap) (set-count bps)
      ;         v (set-count (set-intersect ap bps)))
      (set-count (set-intersect ap bps))))

  (let* ((ap (canonical-points a))
         (bp (oriented-points b)))
    (let-values ([(ad am) (make-delta-set ap)]
                 [(bd bm) (make-delta-set bp)])
      (let* ((di (set-intersect ad bd))
             (ae (or (set-empty? di) (hash-ref am (set-first di))))
             (be (or (set-empty? di) (hash-ref bm (set-first di)))))
        (if (and (list? ae) (list? be))
            (update-location b (point- (first ae) (first be)))
            #f)))))

(define/contract (locate-first srs)
  (-> (listof oriented-scan-result?) (listof oriented-scan-result?))
  (cons (update-location (car srs) '(0 0 0)) (cdr srs)))

(define/contract (locate-some srs)
  (-> (listof oriented-scan-result?) (listof oriented-scan-result?))
  ; (printf "ls ~a~n" (map scan-result-location srs))
  (map
    (lambda (lu)
      (if (located? lu)
          lu
          (or (ormap (curryr match-scan-result-location lu)
                     (filter located? srs))
              lu)))
    srs))

(define/contract (locate-all srs)
  (-> (listof oriented-scan-result?) (listof located-scan-result?))
  (let loop ((srs (locate-first srs)))
    (if (andmap located? srs)
        srs
        (let ((nsrs (locate-some srs)))
          (if (equal? srs nsrs)
              (error "stuck")
              (loop nsrs))))))

(define/contract count-unique-points
  (-> (listof located-scan-result?) integer?)
  (compose set-count (curry apply set-union) (curry map canonical-points)))

(define/contract (furthest-scanners lr)
  (-> (listof located-scan-result?) integer?)

  (define/contract (manhattan a b)
    (-> point3? point3? integer?)
    (sum (map abs (point- a b))))

  (let ((sps (map scan-result-location lr)))
    (let ((mds (map (curry apply manhattan) (combinations sps 2))))
      (apply max mds))))

(define solve
  (lambda (in)
    (let ((lr (locate-all (orient-all in))))
      (cons
        (count-unique-points lr)
        (furthest-scanners lr)))))

(solve! 19 parse solve)
