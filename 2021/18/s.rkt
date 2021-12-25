#lang racket

(require "../../advent.scm")

; Day 18: Snailfish Numbers!

(define snail?
  (flat-rec-contract snail?
    (or/c integer? (cons/c snail? snail?))))

(define/contract parse
  (-> (listof string?) (listof (and/c snail? pair?)))
  (curry map
    ; <sickos> ha ha ha... YES!
    (compose read
             open-input-string
             (curryr string-replace "," " . "))))

; If any pair is nested inside four pairs, the leftmost such pair explodes;
; if any regular number is 10 or greater, the leftmost such regular number
; splits.
;
; To explode a pair, add its left value to the first regular number to the
; left of it (if any), and its right value to the first regular number to the
; right of it. Then, replace the entire exploding pair with the regular
; number 0.
;
; To split a regular number, replace it with a pair: the left element should be
; the number divided by two rounded down and the right element should be the
; number divided by two rounded up.
;
; Any given reduction step is "try to explode, then try to split", and a number
; is reduced when neither of these rules apply.
;
; To implement that:
; The split rule is nicely local but the explode rule is not - it's not clear
; how to write a nice recursive formulation of "first regular number to the
; left" of where we happen to be currently searching. Therefore, I think we
; need to keep track of the path we used to get to the current node, as a
; sequence of left/right choices. With that in hand, we can search for nearby
; numbers and apply updates to the tree "all in one place".

(define path? (listof boolean?))

(define/contract (succ l e)
  (-> (listof any/c) any/c (or/c any/c #f))
  (let ((t (member e l)))
    (if (and t (> (length t) 1))
        (second t)
        #f)))

(define/contract (pred l e)
  (-> (listof any/c) any/c (or/c any/c #f))
  (succ (reverse l) e))

(define/contract (sn-reduce tree)
  (-> snail? snail?)

  (define/contract (inorder-paths t)
    (-> snail? (listof path?))
    (if (pair? t)
        (let ((lp (map (curry cons #t) (inorder-paths (car t))))
              (rp (map (curry cons #f) (inorder-paths (cdr t)))))
          (append lp (list '()) rp))
        (list '())))

  (define/contract (tree-ref t p)
    (-> snail? path? snail?)
    (if (null? p)
        t
        (if (car p)
            (tree-ref (car t) (cdr p))
            (tree-ref (cdr t) (cdr p)))))

  (define/contract (tree-update t p f)
    (-> snail? path? procedure? snail?)
    (cond
      [(null? p) (f t)]
      [(equal? (car p) #t) (cons (tree-update (car t) (cdr p) f) (cdr t))]
      [(equal? (car p) #f) (cons (car t) (tree-update (cdr t) (cdr p) f))]
      [else (error "?" t p f)]))

  (define/contract (can-explode? t p)
    (-> snail? path? (or/c path? boolean?))
    (and (>= (length p) 4)
         (pair? (tree-ref t p))
         p))

  (define/contract (explode t e)
    (-> snail? path? snail?)
    (let* ((ps (inorder-paths t))
           (regps (filter (compose integer? (curry tree-ref t)) ps))
           (lp (pred regps (append e '(#t))))
           (rp (succ regps (append e '(#f))))
           (ev (tree-ref t e))
           (t (tree-update t e (const 0)))
           (t (if lp (tree-update t lp (curry + (car ev))) t))
           (t (if rp (tree-update t rp (curry + (cdr ev))) t)))
      t))

  (define/contract (maybe-explode t)
    (-> snail? snail?)
    (let* ((ps (inorder-paths t))
           (e (ormap (curry can-explode? t) ps)))
      (if e (explode t e) t)))

  (define/contract (split t e)
    (-> snail? path? snail?)
    (let ((ev (tree-ref t e)))
      (tree-update t e (const (cons (floor (/ ev 2)) (ceiling (/ ev 2)))))))

  (define/contract (maybe-split t)
    (-> snail? snail?)
    (let* ((ps (inorder-paths t))
           (regps (filter (compose integer? (curry tree-ref t)) ps))
           (cps (filter (compose (curryr >= 10) (curry tree-ref t)) regps)))
      (if (not (null? cps))
          (split t (first cps))
          t)))

  (let ((et (maybe-explode tree)))
    (if (equal? et tree)
        (maybe-split tree)
        et)))

(define/contract (settle f v)
  (-> procedure? any/c any/c)
  (let ((nv (f v)))
    (if (equal? nv v)
        v
        (settle f nv))))

(define/contract (sn-add a b)
  (-> snail? snail? snail?)
  (settle sn-reduce (cons a b)))

(define/contract (sn-sum ls)
  (-> (listof snail?) snail?)
  (if (= (length ls) 1)
      (first ls)
      (sn-sum (cons (sn-add (first ls) (second ls)) (drop ls 2)))))

(define/contract (magnitude t)
  (-> snail? integer?)
  (if (pair? t)
      (+ (* 3 (magnitude (car t)))
         (* 2 (magnitude (cdr t))))
      t))

(define/contract (biggest-magnitude ts)
  (-> (listof snail?) integer?)
  (let ((ap (append (combinations ts 2) (combinations (reverse ts) 2))))
    (magnitude (sn-sum (argmax (compose magnitude sn-sum) ap)))))

(define solve
  (fork
    (compose magnitude sn-sum)
    biggest-magnitude))

(solve! 18 parse solve)
