#lang racket

(provide icvm make-icvm icvm? icvm-mem icvm-pc icvm-halted
         icvm-copy
         icvm-memref icvm-memset!
         icvm-step! icvm-run!)

(struct icvm (mem pc halted imap) #:mutable)

(define (icvm-memref ic i)
  (vector-ref (icvm-mem ic) i))

(define (icvm-memset! ic i v)
  (vector-set! (icvm-mem ic) i v))

(define (icvm-copy ic)
  (icvm (vector-copy (icvm-mem ic))
        (icvm-pc ic)
        (icvm-halted ic)
        (icvm-imap ic)))

(define (op-unimpl vm)
  (set-icvm-halted! vm #t))

(define (op-halt vm)
  (set-icvm-halted! vm #t))

(define (op-add vm a b c)
  (icvm-memset! vm c (+ (icvm-memref vm a) (icvm-memref vm b))))

(define (op-mul vm a b c)
  (icvm-memset! vm c (* (icvm-memref vm a) (icvm-memref vm b))))

(define (build-imap)
  (let ((v (build-vector 100 (const op-unimpl))))
    (vector-set! v 1 op-add)
    (vector-set! v 2 op-mul)
    (vector-set! v 99 op-halt)
    v))

(define/contract (make-icvm mem)
  (-> (vectorof integer?) icvm?)
  (let ((m (build-vector 8192 (const 0))))
    (vector-copy! m 0 mem)
    (icvm m 0 #f (build-imap))))

(define (icvm-step! vm)
  (define (fetch-args vm pc a)
    (vector->list (vector-copy (icvm-mem vm) (add1 pc) (+ (add1 pc) a))))

  (define (dispatch! vm pc in)
    (let* ((a (sub1 (procedure-arity in)))
           (args (fetch-args vm pc a)))
      (set-icvm-pc! vm (+ pc a 1))
      (apply in vm args)))

  (let ((pc (icvm-pc vm)))
    (if (or (< pc 0)
            (>= pc (vector-length (icvm-mem vm)))
            (< (icvm-memref vm pc) 0)
            (>= (icvm-memref vm pc) (vector-length (icvm-imap vm))))
        (set-icvm-halted! vm #t)
        (let ((in (vector-ref (icvm-imap vm)
                              (icvm-memref vm pc))))
          (dispatch! vm pc in))))
  vm)

(define (icvm-run! vm)
  (do () ((icvm-halted vm))
    (icvm-step! vm)))
