#lang racket

(require "../../advent.rkt")

; Day 8: 7 Segment Search
; We have 7-segment displays with this mapping:
;   abcefg => 0, cf => 1,     acdeg => 2, acdfg => 3,   bcdf => 4,
;   abdfg => 5,  abdefg => 6, acf => 7,   abcdefg => 8, abcdfg => 9
; But the wires are mixed up, so there is some one-to-one mapping being
; applied to the wires in each input. The mapping is different per input.
;
; Each input is 10 digits being displayed (in some random order), then 4 output
; values for this display. It looks like individual signal values are not
; necessarily in order - eg, you can see "cdfeb" or "cdfbe" to mean the same
; thing.
;
; However, part a is a bit simpler: find all the 1s, 4s, 7s, and 8s. Since
; those each use unique numbers of segments it's not too hard.
;
; So, to start off with we'll parse the input into a pair of lists: the ten
; unique digit displays on the left, and the four output displays on the
; right. We'll make the characters in each individual one into symbols, too.

(define (display? v)
  (and (list? v)
       (let ((l (first v)) (r (second v)))
         (and (list? l)
              (= 10 (length l))
              (andmap (listof symbol?) l)
              (list? r)
              (= 4 (length r))
              (andmap (listof symbol?) r)))))

(define/contract (parse ls)
  (-> (listof string?) (listof display?))
  (define/contract (string->symbols s)
    (-> string? (listof symbol?))
    (map (compose string->symbol string) (string->list s)))
  (define/contract (parse-one l)
    (-> string? display?)
    (let* ((p (string-split l "|"))
           (lp (string-split (first p)))
           (rp (string-split (second p))))
      (list
        (map (compose (curryr sort symbol<?) string->symbols) lp)
        (map string->symbols rp))))
  (map parse-one ls))

(define (unique-length? d)
  (case d
    [(2 3 4 7) #t]
    [else #f]))

(define/contract (part-a ds)
  (-> (listof display?) integer?)
  (sum
    (map (curry count (compose unique-length? length))
         (map first ds))))

(define/contract (symbol-count d s)
  (-> display? symbol? integer?)
  (sum (map (curry count (curry symbol=? s)) (first d))))

(define/contract (symbols-with-count d c)
  (-> display? integer? (listof symbol?))
  (filter
    (compose (curryr = c)
             (curry symbol-count d))
    '(a b c d e f g)))

(define/contract (find-a d)
  (-> display? symbol?)
  (let ((seven (first (filter (compose (curry = 3) length) (first d))))
        (one (first (filter (compose (curry = 2) length) (first d)))))
    (set-first (set-subtract seven one))))

(define/contract (find-count-except d c a)
  (-> display? integer? symbol? symbol?)
  (set-first (set-remove (symbols-with-count d c) a)))

(define/contract (find-d d cs)
  (-> display? (listof symbol?) symbol?)
  (let ((ze (filter (curry subset? (list->set cs)) (map list->set (first d)))))
    (set-first (set-symmetric-difference (first ze) (second ze)))))

; Given a single display, make an alist mapping symbols to their real
; display values.
(define/contract (build-mapping d)
  (-> display? list?)
  ; Since we know that the left side has all ten digits represented,
  ; and in particular that it contains:
  ;   1 = cf
  ;   7 = acf
  ; Also, here's how often each symbol should appear total:
  ;   a: 8, b: 6, c: 8, d: 7, e: 4, f: 9, g: 7
  ; So we can easily find b, e & f, and we can find a by finding the symbol
  ; represented in 7 but not 1, which also lets us find c.
  ; Now we know a, c, e, and f, which all appear together only in 0 and 8;
  ; we find those two digits, and their difference gives us d. The other
  ; symbol with 7 appearances is g. That completes our mapping!
  (let* ((cb (first (symbols-with-count d 6)))
         (ce (first (symbols-with-count d 4)))
         (cf (first (symbols-with-count d 9)))
         (ca (find-a d))
         (cc (find-count-except d 8 ca))
         (cd (find-d d (list ca cc ce cf)))
         (cg (find-count-except d 7 cd)))
    (list (cons ca 'a) (cons cb 'b) (cons cc 'c) (cons cd 'd)
          (cons ce 'e) (cons cf 'f) (cons cg 'g))))

(define/contract (unscramble d)
  (-> display? any/c)

  (define/contract (unscramble-symbols m d)
    (-> (listof (cons/c symbol? symbol?)) (listof symbol?) (listof symbol?))
    (map (compose cdr (curryr assoc m)) d))

  (define/contract (symbols->digit d)
    (-> (listof symbol?) integer?)
    (let ((s (list->set d)))
      (cond
        [(equal? s (set 'a 'b 'c 'e 'f 'g)) 0]
        [(equal? s (set 'c 'f)) 1]
        [(equal? s (set 'a 'c 'd 'e 'g)) 2]
        [(equal? s (set 'a 'c 'd 'f 'g)) 3]
        [(equal? s (set 'b 'c 'd 'f)) 4]
        [(equal? s (set 'a 'b 'd 'f 'g)) 5]
        [(equal? s (set 'a 'b 'd 'e 'f 'g)) 6]
        [(equal? s (set 'a 'c 'f)) 7]
        [(equal? s (set 'a 'b 'c 'd 'e 'f 'g)) 8]
        [(equal? s (set 'a 'b 'c 'd 'f 'g)) 9]
        [else (error "?" d)])))

  (let* ((m (build-mapping d))
         (r (map (compose symbols->digit (curry unscramble-symbols m))
                 (second d))))
    (string->number (string-join (map number->string r) ""))))

(define solve
  (fork
    (compose sum
             (curry map (curry count (compose unique-length? length)))
             (curry map second))
    (compose
       sum
       (curry map unscramble))))

(solve! 8 parse solve)
