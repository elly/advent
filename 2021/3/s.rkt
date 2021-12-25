#lang racket

; Day 3: Binary Diagnostic
; Diagnostic report is a list of binary numbers to decode
; From the inputs, generate gamma & epsilon
; The kth bit of gamma is the most common value of the kth bits of the inputs
; The kth bit of epsilon is the least common value of the kth bits of the inputs
; So, gamma is the bitflip of epsilon.
;
; Part 2: oxygen generator rating * co2 scrubber rating
; The oxygen generator rating is the only value for which every bit is the
; most common bit at that position; the co2 scrubber rating is the only value
; for which every bit is the least common bit. If there are ties, for
; most common take the ones with 1, and for least common take the ones with 0.

(require "../../advent.scm")

(define parse
  (curry map
    (lambda (n)
      (map
        (curry char=? #\1)
        (string->list n)))))

(define (ccs vs i)
  (let ((cs (map (curryr list-ref i) vs)))
    (cons (count identity cs)
          (count (negate identity) cs))))

(define (mcc vs i)
  (let ((cc (ccs vs i)))
    (> (car cc) (cdr cc))))

(define lcc (negate mcc))

(define (has-bit? i b x)
  (eq? (list-ref x i) b))

(define (bits->integer bits)
  (string->number
    (list->string
      (map (lambda (b) (if b #\1 #\0)) bits))
    2))

(define (gamma vs)
  (bits->integer
    (build-list (length (first vs))
                (curry mcc vs))))

(define (epsilon vs)
  (let ((g (gamma vs)))
    (bitwise-xor g (- (expt 2 (length (car vs))) 1))))

(define (filter-down vs p)
  (let loop ((vs vs) (i 0))
    (if (= 1 (length vs))
        (car vs)
        (let* ((cc (ccs vs i)))
          (loop (filter (curry p cc i) vs)
                (add1 i))))))

(define (o2gen vs)
  (bits->integer
    (filter-down vs
      (lambda (cc i v)
        (has-bit? i (>= (car cc) (cdr cc)) v)))))

(define (co2scr vs)
  (bits->integer
    (filter-down vs
      (lambda (cc i v)
        (has-bit? i (< (car cc) (cdr cc)) v)))))

(define (pfork f g)
  (lambda args
    (* (apply f args)
       (apply g args))))

(define solve
  (fork
    (pfork gamma epsilon)
    (pfork o2gen co2scr)))

(solve! 3 parse solve)
