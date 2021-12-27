#lang racket

(provide today)
(require srfi/60)
(require "../../lib/list.rkt")

; Day 16: Packet Decoder

; We are given a hex transmission, which is a single packet. A packet is:
;   3 bits of version
;   3 bits of type
;
; Type 4 is a literal, in which case the rest of the packet (the payload) is
; an indefinite-length integer. Break it into 5-bit chunks. The high bit of
; each chunk is used to indicate whether there is a next chunk, and the low
; four bits are the data.
;
; Every other type of packet is an 'operator', which starts with a single bit:
; a length mode. Length mode 0 means the next 15 bits are the length in bits of
; the packet's payload. Length mode 1 means the next 11 bits are the number of
; sub-packets in the packet.
;
; So, the essence of this problem is going to be parsing, at least for now.

; We're especially going to want integer->list and list->integer.

(define *literal-type* 4)

(define bits? (listof boolean?))

(define packet?
  (flat-rec-contract packet?
    (or/c
      (list/c 'lit integer? integer? integer?)
      (list/c 'op integer? integer? (listof packet?)))))

(define packet-tag first)
(define packet-ver second)
(define packet-type third)
(define packet-val fourth)

(define/contract (read-bit s)
  (-> bits? (values boolean? bits?))
  (values (car s) (cdr s)))

(define/contract (read-int s n)
  (-> bits? integer? (values integer? bits?))
  (values (list->integer (take s n))
          (drop s n)))

(define/contract (read-litval s)
  (-> bits? (values integer? bits?))
  (let loop ((v 0) (s s))
    (let*-values ([(k s) (read-bit s)]
                  [(nv s) (read-int s 4)])
      (if k
          (loop (+ (* v 16) nv) s)
          (values (+ (* v 16) nv) s)))))

(define/contract (read-op-payload s)
  (-> bits? (values (listof packet?) bits?))

  (define/contract (read-bits-payload os n)
    (-> bits? integer? (values (listof packet?) bits?))
    (let loop ((ps '()) (s (take os n)))
      (if (null? s)
          (values (reverse ps) (drop os n))
          (let-values ([(np s) (read-packet s)])
            (loop (cons np ps) s)))))

  (define/contract (read-packets-payload s n)
    (-> bits? integer? (values (listof packet?) bits?))
    (let loop ((ps '()) (s s) (n n))
      (if (= 0 n)
          (values (reverse ps) s)
          (let-values ([(np s) (read-packet s)])
            (loop (cons np ps) s (sub1 n))))))

  (let-values ([(lt s) (read-bit s)])
    (if lt
      (let-values ([(np s) (read-int s 11)])
         (read-packets-payload s np))
      (let-values ([(nb s) (read-int s 15)])
         (read-bits-payload s nb)))))

(define/contract (read-packet s)
  (-> bits? (values packet? bits?))
  (let*-values ([(v s) (read-int s 3)]
                [(t s) (read-int s 3)])
    (if (= t *literal-type*)
        (let-values ([(p s) (read-litval s)])
                    (values (list 'lit v t p) s))
        (let-values ([(p s) (read-op-payload s)])
                    (values (list 'op v t p) s)))))

(define (assert-consumed p s)
  p)

(define/contract (pad-to-nibble b)
  (-> bits? bits?)
  (if (or (= (length b) 0)
          (not (= 0 (modulo (length b) 4))))
      (pad-to-nibble (cons #f b))
      b))

(define/contract (hex->bits s)
  (-> string? bits?)

  (define/contract hexd->bits
    (-> string? bits?)
    (compose pad-to-nibble integer->list (curryr string->number 16)))

  (flatten
    (map hexd->bits
      (filter non-empty-string? (string-split s "")))))

(define/contract parse
  (-> (listof string?) any/c)
  (compose assert-consumed
           read-packet
           pad-to-nibble
           hex->bits
           first))

(define/contract (sum-versions p)
  (-> packet? integer?)
  (if (symbol=? (packet-tag p) 'lit)
      (packet-ver p)
      (+ (packet-ver p) (sum (map sum-versions (packet-val p))))))

(define/contract (eval-p p)
  (-> packet? integer?)
  (let* ((v (packet-val p))
         (vs (if (list? v) (map eval-p v) (list v))))
    (case (packet-type p)
      [(0) (sum vs)]
      [(1) (prod vs)]
      [(2) (apply min vs)]
      [(3) (apply max vs)]
      [(4) v]
      [(5) (if (> (first vs) (second vs)) 1 0)]
      [(6) (if (< (first vs) (second vs)) 1 0)]
      [(7) (if (= (first vs) (second vs)) 1 0)])))

(define today (list parse identity sum-versions eval-p (const #t)))
