#lang racket

(require "../../advent.rkt")

; Day 10: Syntax Scoring

(define parse identity)

(define *delims* '((#\( . #\))
                   (#\[ . #\])
                   (#\{ . #\})
                   (#\< . #\>)))

(define *delim-scores-a* '((#\) . 3)
                           (#\] . 57)
                           (#\} . 1197)
                           (#\> . 25137)))

(define *delim-scores-b* '((#\) . 1)
                           (#\] . 2)
                           (#\} . 3)
                           (#\> . 4)))

(define delim-for
  (compose cdr (curryr assoc *delims*)))

(define delim-begin?
  (curryr member (map car *delims*)))
(define delim-end?
  (curryr member (map cdr *delims*)))

(define delim-score-a
  (compose cdr (curryr assoc *delim-scores-a*)))

(define delim-score-b
  (compose cdr (curryr assoc *delim-scores-b*)))

(define/contract (first-unmatched l)
  (-> string? (or/c char? #f list?))
  (let loop ((s '()) (cs (string->list l)))
    (cond
      [(and (null? cs) (null? s)) #f]
      [(null? cs) s]
      [(delim-begin? (car cs))
       (loop (cons (car cs) s) (cdr cs))]
      [(and (delim-end? (car cs)) (null? s)) #f]
      [(and (delim-end? (car cs))
            (char=? (car cs) (delim-for (car s))))
       (loop (cdr s) (cdr cs))]
      [(delim-end? (car cs)) (car cs)]
      [else (loop s (cdr cs))])))

(define (score-completion-string cs)
  (let loop ((s 0) (cs cs))
    (if (null? cs)
        s
        (loop (+ (* s 5) (delim-score-b (car cs))) (cdr cs)))))

(define (middle l)
  (list-ref l (/ (sub1 (length l)) 2)))

(define solve
  (fork
    (compose sum
             (curry map delim-score-a)
             (curry filter char?)
             (curry map first-unmatched))
    (compose middle
             (curryr sort <)
             (curry map score-completion-string)
             (curry map (curry map delim-for))
             (curry filter list?)
             (curry map first-unmatched))))

(solve! 10 parse solve)
