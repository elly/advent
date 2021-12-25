#lang racket

(require "../../advent.scm")

; Day 21: Dirac Dice
; Players play on a circular track with spaces marked 1 through 10; each one has
; a random starting space (from the input). Each turn, each player rolls the die
; 3 times, adds the results, and moves that far forward, then gains points equal
; to the value of the square they stopped on. The player who reaches 1000 points
; first is the winner, and we're asked to compute the losing player's score
; times the number of times the die had been rolled.

; A player's state is their score and position:
(struct player (score pos) #:transparent)

; The dice's state is what it will next roll and how many times it was rolled:
(struct dice (next rolls) #:transparent)

; The input is the two players' starting positions:
(define/contract parse
  (-> (listof string?) (listof player?))
  (curry map (compose (curry player 0) s->i fifth (curryr string-split " "))))

(define/contract (wrap n m)
  (-> integer? integer? integer?)
  (+ 1 (modulo (- n 1) m)))

(define (roll-practice d)
  (values (dice-next d)
          (dice (wrap (add1 (dice-next d)) 100)
                (add1 (dice-rolls d)))))

(define/contract (roll-n r n d)
  (-> procedure? integer? dice? (values integer? dice?))
  (let loop ((n n) (v 0) (d d))
    (if (= n 0)
        (values v d)
        (let-values ([(z d) (r d)])
          (loop (sub1 n) (+ v z) d)))))

(define/contract (advance-player p r)
  (-> player? integer? player?)
  (let ((np (wrap (+ (player-pos p) r) 10)))
    (player (+ (player-score p) np) np)))

(define/contract (take-turn d p)
  (-> dice? player? (values dice? player?))
  (let-values ([(r d) (roll-n roll-practice 3 d)])
    (values d (advance-player p r))))

(define (winner? p)
  (>= (player-score p) 1000))

(define/contract (play-once d ps)
  (-> dice? (listof player?) (values dice? (listof player?)))
  (if (null? ps)
      (values d ps)
      (let-values ([(d np) (take-turn d (car ps))])
        (if (winner? np)
            (values d (cons np (cdr ps)))
            (let-values ([(d rps) (play-once d (cdr ps))])
              (values d (cons np rps)))))))

(define/contract (score d ps)
  (-> dice? (listof player?) integer?)
  (let ((loser (first (filter-not winner? ps))))
    (* (player-score loser) (dice-rolls d))))

(define/contract (play ps)
  (-> (listof player?) integer?)
  (let loop ((d (dice 1 0)) (ps ps))
    (let-values ([(d ps) (play-once d ps)])
      (if (ormap winner? ps)
          (score d ps)
          (loop d ps)))))

(define *wc-memotab* (make-hash))

(define/contract (wc p0 p1)
  (-> player? player? (cons/c integer? integer?))
  (define (swap p) (cons (cdr p) (car p)))
  (define (pair* s p) (cons (* (car p) s) (* (cdr p) s)))
  (define (cons+ c0 c1) (cons (+ (car c0) (car c1)) (+ (cdr c0) (cdr c1))))

  (define *ways*
    '((3 . 1) (4 . 3) (5 . 6) (6 . 7) (7 . 6) (8 . 3) (9 . 1)))

  (define (wc-w p0 p1 w)
    (let ((p (wc p1 (advance-player p0 (car w)))))
      (pair* (cdr w) (swap p))))

  (cond
    [(hash-has-key? *wc-memotab* (cons p0 p1))
     (hash-ref *wc-memotab* (cons p0 p1))]
    [(>= (player-score p0) 21) (cons 1 0)]
    [(>= (player-score p1) 21) (cons 0 1)]
    [else
      (begin
        (let ((s (foldl cons+ '(0 . 0) (map (curry wc-w p0 p1) *ways*))))
          (hash-set! *wc-memotab* (cons p0 p1) s)
          s))]))

(define/contract (quantum-play ps)
  (-> (listof player?) integer?)
  (let ((r (wc (first ps) (second ps))))
    (if (> (car r) (cdr r)) (car r) (cdr r))))

(define solve
  (fork
    play
    quantum-play))

(solve! 21 parse solve)
