#lang racket

(require "../../advent.rkt")

(define/contract parse
  (-> (listof string?) (listof integer?))
  (compose (curry map s->i)
           (curryr string-split ",")
           car))

(define/contract (cost-for f cs s)
  (-> procedure? (listof integer?) integer? integer?)
  (sum
    (map (curryr f s) cs)))

(define/contract (imedian is)
  (-> (listof integer?) integer?)
  (if (even? (length is))
      (list-ref is (/ (length is) 2))
      (list-ref is (/ (+ (length is) 1) 2))))

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
  (let* ((m (imedian cs))
         (cm (cost-for f cs m))
         (cl (cost-for f cs (- m 1)))
         (cr (cost-for f cs (+ m 1))))
    (cond
      [(and (>= cl cm) (>= cr cm)) cm]
      [(< cl cm) (searchmin f cs m -1)]
      [(< cr cm) (searchmin f cs m 1)]
      [else (error "?")])))

(define/contract (summat n)
  (-> integer? integer?)
  (/ (* n (+ n 1)) 2))

(define solve
  (fork
    (curry findmin
      (lambda (p c) (abs (- p c))))
    (curry findmin
      (lambda (p c) (summat (abs (- p c)))))))

(solve! 7 parse solve)
