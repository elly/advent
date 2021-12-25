#lang racket

(require "../advent.scm")

; Day 23: Amphipod
;
; We have a little map of a maze and creatures (amphipods) that want to move
; around in the maze. From our initial state, we want to get to:
;   #############
;   #...........#
;   ###A#B#C#D###
;     #A#B#C#D#
;     #########
; i.e., reorder all the amphipods into left-to-right rooms like that.
;
; The top area is the "hallway", the bottom areas are the "rooms". The spaces
; in the hallway directly above the rooms are the entrances. There are some
; constraints on how the amphipods move:
; 1. They will never end their move on an entrance
; 2. They will never move from the hallway to a room unless:
;    a. That is their destination room, and
;    b. That room contains no amphipods which do not have it as their
;       destination
; 3. They will never move hallway->hallway
;
; As a result, each amphipod takes a max of 2 turns:
; * Moving itself from room to a hallway square
; * Moving itself from that hallway square to its destination room
;
; A solution is a sequence of amphipod moves that transform the initial state
; (the input) into the final state given above, and we're looking for the
; solution with the lowest cost, given that it costs 1 to move an A amphipod one
; space, 10 to move a B amphipod, 100 to move a C amphipod, and 1000 to move a
; D amphipod.

(define map? vec2?)
(define point? (cons/c integer? integer?))
(define state? (cons/c map? (hash/c point? symbol?)))

(define/contract (parse ls)
  (-> (listof string?) state?)

  (define/contract (build-map ls)
    (-> (listof string?) map?)
    (list->vector
      (map
        (lambda (rs)
          (list->vector (map (curry char=? #\#) (string->list rs))))
      ls)))

  (define/contract (find-amphs ls)
    (-> (listof string?) (hash/c point? symbol?))
    (let yloop ((as (hash)) (y 0))
      (if (= y (length ls))
          as
          (let xloop ((as as) (x 0))
            (if (= x (string-length (list-ref ls y)))
                (yloop as (add1 y))
                (let ((c (string-ref (list-ref ls y) x)))
                  (if (char-alphabetic? c)
                      (xloop (hash-set as (cons x y)
                                          (string->symbol (string c)))
                             (add1 x))
                      (xloop as (add1 x)))))))))
  (cons
    (build-map ls)
    (find-amphs ls)))

(define (dest-for t)
  (case t
    ((A) 3) ((B) 5) ((C) 7) ((D) 9)))

(define (in-dest? a p)
  (case a
    ((A) (or (equal? p '(3 . 2)) (equal? p '(3 . 3))))
    ((B) (or (equal? p '(5 . 2)) (equal? p '(5 . 3))))
    ((C) (or (equal? p '(7 . 2)) (equal? p '(7 . 3))))
    ((D) (or (equal? p '(9 . 2)) (equal? p '(9 . 3))))))

(define (in-room? p)
  (> (cdr p) 1))

(define (in-hallway? p)
  (and (= (cdr p) 1)
       (car p)))

(define (valid-moves-from t as p)

  (define all-hallway-moves
    '((1 . 1) (2 . 1) (4 . 1) (6 . 1) (8 . 1) (10 . 1) (11 . 1)))

  (define blocked-in
    (and (= (cdr p) 3)
         (hash-ref as (cons (car p) 2) #f)))

  (define (bmin bs)
    (if (null? bs) 99 (apply min bs)))
  (define (bmax bs)
    (if (null? bs) -99 (apply max bs)))

  (define (valid-hallway-move? bs m)
    (and (> (car m) (cdr bs)) (< (car m) (car bs))))

  (define (blockers)
    (let* ((bs (filter-map in-hallway? (hash-keys as)))
           (rb (bmin (filter (curry < (car p)) bs)))
           (lb (bmax (filter (curry > (car p)) bs))))
      (cons rb lb)))

  (define (valid-hallway-moves)
      (list->set
         (filter (curry valid-hallway-move? (blockers))
                 all-hallway-moves)))

  (define (dest-moves a)
    (let* ((f (dest-for a))
           (fh (cons f 2))
           (fl (cons f 3))
           (ah (hash-ref as (cons f 2) #f))
           (al (hash-ref as (cons f 3) #f)))
      (if (or (and ah (not (in-dest? ah fh)))
              (and al (not (in-dest? al fl))))
          (list)
          (if (not al)
              (list fl)
              (list fh)))))

  (define (valid-dest-moves a)
      (list->set
          (filter (curry valid-hallway-move? (blockers))
                  (dest-moves a))))

  (define (in-filled-dest? a p)
    (let* ((fh (cons (car p) 2)) (fl (cons (car p) 3))
           (ah (hash-ref as fh #f)) (al (hash-ref as fl #f)))
      (or (and (= (cdr p) 3) (in-dest? a p))
          (and (= (cdr p) 2) (in-dest? a p) (in-dest? al fl)))))

  (let ((a (hash-ref as p)))
    (cond
      [(in-filled-dest? a p) (set)]
      [blocked-in (set)]
      [(in-room? p) (valid-hallway-moves)]
      [else (valid-dest-moves a)])))

(define (apply-move as m)
  (hash-remove
    (hash-set as (cdr m) (hash-ref as (car m)))
    (car m)))

(define solution? (listof (cons/c point? point?)))

(define (move-cost as m)
  (define tm
    (case (hash-ref as (car m))
      ((A) 1) ((B) 10) ((C) 100) ((D) 1000)))
  (* tm (+ (abs (- (car (car m)) (car (cdr m))))
           (abs (- (cdr (car m)) (cdr (cdr m)))))))

(define (solution t as mc)

  (define (all-valid-moves)
    (sort
      (filter
        (lambda (m) (< (move-cost as m) mc))
        (apply append
          (map
            (lambda (asp)
              (let ((ms (valid-moves-from t as asp)))
                (set-map ms (curry cons asp))))
            (hash-keys as))))
      <
      #:key (curry move-cost as)))

  (define (final?)
    (andmap (lambda (p) (in-dest? (cdr p) (car p))) (hash->list as)))

  (define (solution-after t as m mc)
    (let* ((nas (apply-move as m))
           (ss (solution t nas (- mc (move-cost as m)))))
      (+ (move-cost as m) (solution t nas (- mc (move-cost as m))))))

  (if (final?)
    0
    (let loop ((ms (all-valid-moves)) (mc mc))
      (if (null? ms)
          mc
          (let ((sc (solution-after t as (car ms) mc)))
             (loop (cdr ms) (min sc mc)))))))

(define solve
  (fork
    (lambda (s) (solution (car s) (cdr s) 14502))
    (const 0)))

(solve! 23 parse solve) 
