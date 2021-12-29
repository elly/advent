#lang racket

(provide today)

(define (parse ls)
  (let ((ss (string-split (first ls) "-")))
    (cons (string->number (first ss))
          (string->number (second ss)))))

(define (no-decrease? cl)
  (if (< (length cl) 2)
      #t
      (if (char>? (first cl) (second cl))
          #f
          (no-decrease? (cdr cl)))))

(define (freq-counts l)
  (map length (group-by identity l)))

(define (ok-a? pw)
  (and (no-decrease? pw)
       (>= (apply max (freq-counts pw)) 2)))

(define (ok-b? pw)
  (and (no-decrease? pw)
       (member 2 (freq-counts pw))))

(define (extract r)
  (build-list (- (add1 (cdr r)) (car r))
              (compose string->list number->string (curry + (car r)))))

(define solve-a (curry count ok-a?))
(define solve-b (curry count ok-b?))

(define today (list parse extract solve-a solve-b (const #t)))
