#lang racket

(provide iterate-n iterate-until)

; Given a function, a count, and a value, repeatedly apply that function
; to that value that many times. For example:
;   (iterate-n sub1 3 10)
; would be 7.
(define/contract (iterate-n f n x)
  (-> procedure? integer? any/c any/c)
  (let loop ((n n) (x x))
    (if (= n 0)
        x
        (loop (sub1 n) (f x)))))

; Given a function, a predicate, and a value, iterate that function until the
; predicate returns true on the old value and the new value.
(define/contract (iterate-until f p x)
  (-> procedure? procedure? any/c any/c)
  (let loop ((x x))
    (let ((nx (f x)))
      (if (p x nx)
          x
          (loop nx)))))
