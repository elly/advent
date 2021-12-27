#lang racket

(provide project stanza sum prod)

(define (project l . fs)
  (map (lambda (f e) (f e)) fs l))

(define/contract (stanza sp lst)
  (-> procedure? (listof any/c) (listof (listof any/c)))
  (let loop ((r '()) (lst lst))
    (cond
      [(null? lst) (if r (list (reverse r)) '())]
      [(sp (car lst)) (cons (reverse r) (stanza sp (cdr lst)))]
      [else (loop (cons (car lst) r) (cdr lst))])))

(define sum (curry foldl + 0))
(define prod (curry foldl * 1))
