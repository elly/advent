#lang racket

(provide iterate-n iterate-until)

(define/contract (iterate-n f n x)
  (-> procedure? integer? any/c any/c)
  (let loop ((n n) (x x))
    (if (= n 0)
        x
        (loop (sub1 n) (f x)))))

(define/contract (iterate-until f p x)
  (-> procedure? procedure? any/c any/c)
  (let loop ((x x))
    (let ((nx (f x)))
      (if (p x nx)
          x
          (loop nx)))))
        
