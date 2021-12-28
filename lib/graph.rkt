#lang racket

(provide astar)
(require data/heap)

(define (astar ss es nf hf)
  (define opens (make-heap (lambda (s0 s1) (<= (cdr s0) (cdr s1)))))
  (define camefrom (make-hash))
  (define gscore (make-hash (list (cons ss 0))))
  (define done #f)

  (define *infinity* 9999999999)
  (define (gsq s) (hash-ref gscore s *infinity*))

  (heap-add! opens (cons ss (hf (cons ss 0))))

  (do ()
      ((or (= (heap-count opens) 0) done))
    (let* ((c (heap-min opens))
           (cn (car c)) (cc (cdr c)))
      (heap-remove-min! opens)
      (let ((ns (nf (car c))))
        (for/list ((n ns))
          (let ((nn (car n)) (nc (cdr n)))
            (let ((tg (+ (gsq cn) nc)))
              (when (< tg (gsq nn))
                  (begin
                    (hash-set! camefrom nn cn)
                    (hash-set! gscore nn tg)
                    (heap-add! opens (cons nn (+ tg (hf n))))))))))
        (when (equal? (car c) es)
            (set! done #t))))
  (define path
    (let loop ((e es) (p (list)))
      (if (equal? e ss)
          (reverse (cons e p))
          (loop (hash-ref camefrom e) (cons e p)))))
  (values (gsq es) path))
