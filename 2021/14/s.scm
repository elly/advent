#lang racket

(require "../../advent.scm")

; Day 14: Extended Polymerization
; We are given a polymer (a sequence of letters) and a list of rules, which
; each map a pair of letters to a new letter, which should be inserted between
; them. We also define an expansion step, in which every such matching rule is
; applied simultaneously to the input polymer, producing a new, larger one.
; We are asked to compute a simple function of the counts of letters in the
; resulting polymer after a given number of steps.
;
; This is my rewrite, now that I've seen what part B is, with a better data
; structure.

(define polymer? (listof symbol?))
(define polypair? symbol?)

; We represent a rule with the two new pairs being generated on the right
; side - notionally the left side pair is consumed and both right side pairs
; are produced. For example, if:
;   NB -> C
; then applying that rule to NB produces NCB, so the NB pair goes away and
; the NC and CB pairs are created.
(define rule? (list/c polypair? polypair? polypair?))

; A polycount is a count of how many times each polypair appears in the
; polymer we're working on.
(define polycount? hash?)

(define/contract (polypair a b)
  (-> symbol? symbol? polypair?)
  (string->symbol
    (string-join
      (list (symbol->string a) (symbol->string b))
      "")))

(define/contract polypair-left
  (-> polypair? symbol?)
  (compose string->symbol string (curryr string-ref 0) symbol->string))

(define/contract polypair-right
  (-> polypair? symbol?)
  (compose string->symbol string (curryr string-ref 1) symbol->string))

(define/contract (parse ls)
  (-> (listof string?) (cons/c polymer? (listof rule?)))

  (define/contract (parse-polymer p)
    (-> string? (or/c polymer? #f))
    (and (not (string-contains? p "->"))
         (map (compose string->symbol string) (string->list p))))

  (define/contract (parse-rule p)
    (-> string? (or/c rule? #f))
    (and (string-contains? p "->")
         (let* ((ps (string-split p " -> "))
                (lhs (parse-polymer (first ps)))
                (rhs (parse-polymer (second ps)))
                (la (first lhs))
                (lb (second lhs))
                (r (first rhs)))
           (list (polypair la lb) (polypair la r) (polypair r lb)))))

  (cons
    (first (filter-map parse-polymer ls))
    (filter-map parse-rule ls)))

(define/contract (count-pairs p)
  (-> polymer? polycount?)
  (if (< (length p) 2)
      (hash)
      (hash-update (count-pairs (cdr p))
                   (polypair (first p) (second p))
                   add1
                   0)))

; When expanding a polymer (represented as a polycount) with a set of rules,
; we iterate across the set of rules, generating a second polycount of deltas
; to apply to the original polycount. For example, if we have two rules:
;   (AA AB BA)
;   (CA CA AA)
; and our input polycount includes:
;   (AA 2) (CA 1) (BB 1)
; then we could apply the AA rule twice and the CA rule once, producing:
;   (AB 2) (BA 2) (AA -2) (CA 1) (AA 1) (CA -1) (BB 1)
; and so we'd end up with:
;   (AB 2) (BA 2) (AA 1) (CA 1) (BB 1)
(define/contract (expand-delta rs pc)
  (-> (listof rule?) polycount? polycount?)

  (define/contract (expand-one o r ds)
    (-> polycount? rule? polycount? polycount?)
    (let-values (((l ra rb) (apply values r)))
      (let ((n (hash-ref o l 0)))
        (if (> n 0)
          ((compose
             (curryr hash-update l (curryr - n) 0)
             (curryr hash-update ra (curry + n) 0)
             (curryr hash-update rb (curry + n) 0))
           ds)
          ds))))

  (foldl (curry expand-one pc) pc rs))

(define/contract (step p n)
  (-> (cons/c polymer? (listof rule?)) integer? polycount?)
  (iterate-n (curry expand-delta (cdr p)) n (count-pairs (car p))))

(define/contract (atom-counts pc)
  (-> polycount? (listof integer?))
  (hash-map
    (foldl
      (lambda (a c)
        (let ((l (polypair-left a)) (r (polypair-right a))
              (v (hash-ref pc a)))
          ((compose
             (curryr hash-update l (curry + v) 0)
             (curryr hash-update r (curry + v) 0))
          c)))
      (hash) (hash-keys pc))
   (lambda (k v)
     (ceiling (/ v 2)))))

(define/contract (poly-score ac)
  (-> (listof integer?) integer?)
  (- (apply max ac) (apply min ac)))

(define/contract (solve-part n in)
  (-> integer? (cons/c polymer? (listof rule?)) integer?)
  (poly-score (atom-counts (step in n))))

(define solve
  (fork
    (curry solve-part 10)
    (curry solve-part 40)))

(solve! 14 parse solve)
