#lang racket

(provide project stanza sum prod)

; This function finds a projection of the supplied list with the supplied
; list of transformations by applying transformations to corresponding elements
; of the list. For example:
;   (project '(1 2 3) add1 sub1 add1)
; would be:
;   '(2 1 4)
(define (project l . fs)
  (map (lambda (f e) (f e)) fs l))

; This function takes a predicate sp and a list lst, and splits lst into
; sublists, separated by elements for which sp returns true. For example,
;   (stanza even? '(1 1 2 3 3 4 5 5 6 7 7))
; would be:
;   '((1 1) (3 3) (5 5) (7 7))
(define/contract (stanza sp lst)
  (-> procedure? (listof any/c) (listof (listof any/c)))
  (let loop ((r '()) (lst lst))
    (cond
      [(null? lst) (if r (list (reverse r)) '())]
      [(sp (car lst)) (cons (reverse r) (stanza sp (cdr lst)))]
      [else (loop (cons (car lst) r) (cdr lst))])))

(define sum (curry foldl + 0))
(define prod (curry foldl * 1))
