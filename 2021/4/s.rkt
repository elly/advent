#lang racket

; Day 4: Giant Squid Bingo!

(provide today)
(require "../../lib/list.rkt")

; Here, boards are represented as 1d vectors of integers:
(define (board? b)
  (and (vector? b)
       (= (vector-length b) 25)
       (= (vector-count integer? b) 25)))

(define/contract (parse-board ls)
  (-> (listof string?) board?)

  (define/contract (parse-board-line l)
    (-> string? (listof integer?))
    (map string->number (string-split (string-normalize-spaces l))))

  (let ((is (map parse-board-line ls)))
    (list->vector (append* is))))

(define/contract (parse lines)
  (-> (listof string?)
      (cons/c (listof integer?) (listof board?)))
  (cons
    (map string->number (string-split (first lines) ","))
    (map parse-board (stanza (curry string=? "") (drop lines 2)))))

(define/contract (board-wins? b v)
  (-> board? (listof integer?) boolean?)
  (let ((w (list->set v)))
    (define (r x) (set-member? w (vector-ref b x)))
    ; This is open-coded because it ends up being more readable than a pair
    ; of loops, at least to me.
    (or
      (and (r 0) (r 1) (r 2) (r 3) (r 4))
      (and (r 5) (r 6) (r 7) (r 8) (r 9))
      (and (r 10) (r 11) (r 12) (r 13) (r 14))
      (and (r 15) (r 16) (r 17) (r 18) (r 19))
      (and (r 20) (r 21) (r 22) (r 23) (r 24))

      (and (r 0) (r 5) (r 10) (r 15) (r 20))
      (and (r 1) (r 6) (r 11) (r 16) (r 21))
      (and (r 2) (r 7) (r 12) (r 17) (r 22))
      (and (r 3) (r 8) (r 13) (r 18) (r 23))
      (and (r 4) (r 9) (r 14) (r 19) (r 24)))))

; Iterate through prefixes of the list of numbers until some board wins on some
; prefix, then return that board, assuming it's the unique winner at this point.
(define/contract (first-winning-board bs v)
  (-> (listof board?) (listof integer?) (cons/c (listof integer?) board?))
  (let loop ((i 0))
    (if (= i (length v))
        (error "no winner?")
        (let ((wbs (filter (curryr board-wins? (take v i)) bs)))
          (if (not (null? wbs))
               (cons (take v i) (car wbs))
               (loop (add1 i)))))))

; Repeatedly filter out boards that have won for given prefixes of v until
; there is only a single board remaining, then use first-winning-board to
; figure out the exact prefix of v it needs to win (for scoring).
(define/contract (last-winning-board bs v)
  (-> (listof board?) (listof integer?) (cons/c (listof integer?) board?))
  (let loop ((i 0) (bs bs))
    (if (= (length bs) 1)
      ; I made a mistake the first time through here, having this just return
      ; the board and v here, but just because we've found the last remaining
      ; board doesn't mean it will win on the very next number. Oops!
      (first-winning-board bs v)
      (loop (add1 i)
            (filter-not (curryr board-wins? (take v i)) bs)))))

; Returns the list of unmarked numbers on b if the numbers in v have been
; called.
(define/contract (unmarked b v)
  (-> board? (listof integer?) (listof integer?))
  (filter-not (curry set-member? (list->set v)) (vector->list b)))

(define (part-a p)
  (let ((v (car p)) (bs (cdr p)))
    (let* ((wbs (first-winning-board bs v))
           (wv (car wbs))
           (wb (cdr wbs)))
      (* (sum (unmarked wb wv)) (last wv)))))

(define (part-b p)
  (let ((v (car p)) (bs (cdr p)))
    (let* ((nwbs (last-winning-board bs v))
           (nwv (car nwbs))
           (nwb (cdr nwbs)))
      (* (sum (unmarked nwb nwv)) (last nwv)))))

(define today (list parse identity part-a part-b (const #t)))
