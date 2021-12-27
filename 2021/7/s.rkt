#lang racket

(provide today)
(require "../../lib/list.rkt"
         "../../lib/num.rkt")

(define/contract parse
  (-> (listof string?) (listof integer?))
  (compose (curry map string->number)
           (curryr string-split ",")
           car))

(define/contract (cost-for f cs s)
  (-> procedure? (listof integer?) integer? integer?)
  (sum
    (map (curryr f s) cs)))

(define/contract (searchmin f cs p d)
  (-> procedure? (listof integer?) integer? integer? integer?)
  (let loop ((p p))
    (let ((v (cost-for f cs p))
          (n (cost-for f cs (+ p d))))
      (if (> n v)
        v
        (loop (+ p d))))))

(define/contract (findmin f cs)
  (-> procedure? (listof integer?) integer?)
  (let* ((m (first cs))
         (cm (cost-for f cs m))
         (cl (cost-for f cs (- m 1)))
         (cr (cost-for f cs (+ m 1))))
    (cond
      [(and (>= cl cm) (>= cr cm)) cm]
      [(< cl cm) (searchmin f cs m -1)]
      [(< cr cm) (searchmin f cs m 1)]
      [else (error "?")])))

(define solve-a (curry findmin (lambda (p c) (abs (- p c)))))
(define solve-b (curry findmin (lambda (p c) (summat (abs (- p c))))))

(define today (list parse identity solve-a solve-b (const #t)))
