#lang racket

(require "day.rkt")

(define (load-all-days)
  (list (dynamic-require "td.rkt" 'today)))

(define (check d)
  (printf "~a~n" d))

(define (main args)
  (let ((days (load-all-days)))
    (let ((verb (vector-ref args 0)))
      (case verb
        [("check") (check (first days))]
        [else (error "?" args)]))))

(main (current-command-line-arguments))
