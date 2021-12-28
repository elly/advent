#lang racket

(provide today)
(require "../../lib/geom.rkt")
(require data/heap)

; Day 23: Amphipod
; We have a little maze-map with some amphipods in it, which are represented by
; capital letters. The amphipods want to rearrange themselves but they can only
; move in certain constrained ways, and have different costs on movement; we
; want to find the cheapest sequence of moves that put the amphipods into their
; desired arrangement.

(define (cost-for a)
  (case a
    ((A) 1) ((B) 10) ((C) 100) ((D) 1000) (else (error "cost-for" a))))

(define (goal-index a)
  (case a
    ((A) 0) ((B) 1) ((C) 2) ((D) 3) (else (error "goal-index" a))))

(define (goal-type i)
  (case i
    ((0) 'A) ((1) 'B) ((2) 'C) ((3) 'D)))

; The map looks like this:
; #############
; #,,x,x,x,x,,#
; ###.#.#.#.###
;   #.#.#.#.#
;   #########
; where the areas marked with '.' are rooms, the areas marked with ',' are
; hallway, and the areas marked with 'x' are entryways.

(define (entryway? x)
  (case x
    ((3 5 7 9) #t)
    (else #f)))

; Amphipods take turns moving, under these constraints:
; 1. They never move through another amphipod
; 2. They never end their move in an entryway
; 3. They never move into a room that is not their destination
; 4. They never move into a room containing amphipods of another type
; 5. If their move starts in a room it ends in a hallway
; 6. If their move starts in a hallway it ends in a room

; We can view this problem as a graph search, where we have a graph of states,
; with an edge between states (s0, s1) with weight w if there is a legal move
; for some amphipod with cost w that moves the state from s0 to s1. We then
; wish to find the shortest path between a start state and the final state.
;
; We'll represent the state as a hallway vector, then a vector of lists for the
; rooms:

(struct state (hallway rooms roomdepth) #:transparent)

(define/contract (parse ls)
  (-> (listof string?) state?)
  (define (l->c s i)
    (string->symbol (string (string-ref s i))))

  (let ((r0 (third ls))
        (r1 (fourth ls)))
    (state (build-vector 13
                         (lambda (i) (or (= i 0) (= i 12))))
           (vector (list (l->c r0 3) (l->c r1 3))
                   (list (l->c r0 5) (l->c r1 5))
                   (list (l->c r0 7) (l->c r1 7))
                   (list (l->c r0 9) (l->c r1 9)))
           2)))

; From a given state, we can compute all the states reachable with one
; move, along with their costs. The states reachable in one move are all those
; which either:
; * Move an amphipod from a room to a hallway, or
; * Move an amphipod from a hallway to a room

(define (state-room s ri)
  (vector-ref (state-rooms s) ri))

(define (room-finished? s ri)
  (andmap (curry symbol=? (goal-type ri)) (state-room s ri)))

(define (room-empty? s ri)
  (= (length (state-room s ri)) 0))

(define (room-mixed? s ri)
  (ormap (lambda (t) (not (= ri (goal-index t))))
         (state-room s ri)))

(define (ri->x ri)
  (+ (* ri 2) 3))

(define (goal-x a)
  (ri->x (goal-index a)))

(define (blocker-for h x d)
  (let loop ((x x))
    (if (or (< x 0) (>= x (vector-length h)) (vector-ref h x))
        x
        (loop (+ x d)))))

(define (valid-hallway-dests s ri)
  (let* ((sx (ri->x ri))
         (rbx (blocker-for (state-hallway s) sx 1))
         (lbx (blocker-for (state-hallway s) sx -1)))
    (let ((r '()))
      (for ((d (- rbx sx)) #:unless (entryway? (+ sx d)))
        (set! r (cons (+ sx d) r)))
      (for ((d (- sx lbx)) #:unless (entryway? (- sx d)))
        (set! r (cons (- sx d) r)))
      r)))

(define (move-cost s d t)
  (* (cost-for t) (point-manhattan s d)))

(define (valid-hallway-moves s ri t rdx)
  (let ((ds (valid-hallway-dests s ri)))
    (map
      (lambda (dx)
        (let ((nh (vector-copy (state-hallway s))))
          (vector-set! nh dx t)
          (cons (struct-copy state s (hallway nh))
                (move-cost (point (ri->x ri) (+ 2 rdx) 0)
                           (point dx 1 0)
                           t))))
    ds)))

(define (room-remove-first s ri)
  (let ((r (state-room s ri))
        (nv (vector-copy (state-rooms s))))
    (vector-set! nv ri (cdr r))
    (values (struct-copy state s (rooms nv))
            (car r)
            (- (state-roomdepth s) (length r)))))

(define (neighbors s)
  (define (room-hallway-moves-from s ri)
    (if (or (room-finished? s ri) (room-empty? s ri))
        (list)
        (let-values (((ns a rdx) (room-remove-first s ri)))
          (valid-hallway-moves ns ri a rdx))))

  (define (room-hallway-moves s)
    (append (room-hallway-moves-from s 0)
            (room-hallway-moves-from s 1)
            (room-hallway-moves-from s 2)
            (room-hallway-moves-from s 3)))

  (define (hallway-room-move s x t)
    (let ((nh (vector-copy (state-hallway s)))
          (nr (vector-copy (state-rooms s)))
          (r (state-room s (goal-index t))))
      (vector-set! nh x #f)
      (vector-set! nr (goal-index t) (cons t r))
      (cons
        (struct-copy state s (hallway nh) (rooms nr))
        (move-cost (point x 1 0)
                   (point (goal-x t)
                          (+ 1 (- (state-roomdepth s) (length r)))
                          0)
                   t))))

  (define (hallway-room-moves-from s x)
    (let* ((h (state-hallway s))
           (lbx (blocker-for h (sub1 x) -1))
           (rbx (blocker-for h (add1 x) 1))
           (t (vector-ref h x)))
      (and t
           (symbol? t)
           (< lbx (goal-x t))
           (> rbx (goal-x t))
           (not (room-mixed? s (goal-index t)))
           (hallway-room-move s x t))))

  (define (hallway-room-moves s)
    (filter identity
            (build-list (vector-length (state-hallway s))
                        (lambda (i)
                          (hallway-room-moves-from s i)))))

  (append (room-hallway-moves s) (hallway-room-moves s)))

(define (heuristic s)
  ; Surely you are joking? This runs basically instantly even with no heuristic
  ; (i.e. in straight Dijkstra mode)
  0)

(define (astar ss es)
  (define opens (make-heap (lambda (s0 s1) (<= (cdr s0) (cdr s1)))))
  (define camefrom (make-hash))
  (define gscore (make-hash (list (cons ss 0))))
  (define done #f)

  (define *infinity* 9999999999)
  (define (gsq s) (hash-ref gscore s *infinity*))

  (heap-add! opens (cons ss (heuristic (cons ss 0))))

  (do ()
      ((or (= (heap-count opens) 0) done))
    (let* ((c (heap-min opens))
           (cn (car c)) (cc (cdr c)))
      (heap-remove-min! opens)
      (let ((ns (neighbors (car c))))
        (for/list ((n ns))
          (let ((nn (car n)) (nc (cdr n)))
            (let ((tg (+ (gsq cn) nc)))
              (when (< tg (gsq nn))
                  (begin
                    (hash-set! camefrom nn cn)
                    (hash-set! gscore nn tg)
                    (heap-add! opens (cons nn (+ tg (heuristic n))))))))))
        (when (equal? (car c) es)
            (set! done #t))))
  (gsq es))

(define extract identity)

(define *final-state-a*
  (state #(#t #f #f #f #f #f #f #f #f #f #f #f #t)
         #((A A) (B B) (C C) (D D))
         2))

(define *final-state-b*
  (state #(#t #f #f #f #f #f #f #f #f #f #f #f #t)
         #((A A A A) (B B B B) (C C C C) (D D D D))
         4))

(define (solve-a ss)
  (astar ss *final-state-a*))

(define (augment-for-b ss)
  (define (rv i) (vector-ref (state-rooms ss) i))
  (struct-copy state ss
    (rooms
      (vector
        (list (first (rv 0)) 'D 'D (second (rv 0)))
        (list (first (rv 1)) 'C 'B (second (rv 1)))
        (list (first (rv 2)) 'B 'A (second (rv 2)))
        (list (first (rv 3)) 'A 'C (second (rv 3)))))
    (roomdepth 4)))

(define (solve-b ss)
  (astar (augment-for-b ss) *final-state-b*))

(define today (list parse extract solve-a solve-b (const #t)))
