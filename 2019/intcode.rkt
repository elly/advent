#lang racket

(provide icvm make-icvm icvm? icvm-mem icvm-pc icvm-halted
         icvm-copy icvm-load
         icvm-memref icvm-memset!
         icvm-step! icvm-run!
         set-icvm-inproc! set-icvm-outproc!
         set-icvm-trace!

         with-fixed-io)

(struct icvm (mem pc halted trace imap inproc outproc) #:mutable)

(define (icvm-memref ic i)
  (vector-ref (icvm-mem ic) i))

(define (icvm-memset! ic i v)
  (vector-set! (icvm-mem ic) i v))

(define (icvm-copy ic)
  (struct-copy icvm ic (mem (vector-copy (icvm-mem ic)))))

(define (iread vm a)
  (case (cdr a)
    ((imm) (car a))
    ((pos) (icvm-memref vm (car a)))
    (else (error "read mode ~a~n" a))))

(define (iwrite! vm a v)
  (case (cdr a)
    ((imm) (error "write to imm ~a~n" a))
    ((pos) (icvm-memset! vm (car a) v))))

(define (op-unimpl vm)
  (set-icvm-halted! vm #t))

(define (op-halt vm)
  (set-icvm-halted! vm #t))

(define (op-add vm a b c)
  (iwrite! vm c (+ (iread vm a) (iread vm b))))

(define (op-mul vm a b c)
  (iwrite! vm c (* (iread vm a) (iread vm b))))

(define (op-in vm a)
  (iwrite! vm a ((icvm-inproc vm))))

(define (op-out vm a)
  ((icvm-outproc vm) (iread vm a)))

(define (op-jt vm a b)
  (when (not (= 0 (iread vm a)))
        (set-icvm-pc! vm (iread vm b))))

(define (op-jf vm a b)
  (when (= 0 (iread vm a))
        (set-icvm-pc! vm (iread vm b))))


(define (op-lt vm a b c)
  (iwrite! vm c (if (< (iread vm a) (iread vm b)) 1 0)))

(define (op-eq vm a b c)
  (iwrite! vm c (if (= (iread vm a) (iread vm b)) 1 0)))

(define (build-imap)
  (let ((v (build-vector 100 (const op-unimpl))))
    (vector-set! v 1 op-add)
    (vector-set! v 2 op-mul)
    (vector-set! v 3 op-in)
    (vector-set! v 4 op-out)
    (vector-set! v 5 op-jt)
    (vector-set! v 6 op-jf)
    (vector-set! v 7 op-lt)
    (vector-set! v 8 op-eq)
    (vector-set! v 99 op-halt)
    v))

(define/contract (make-icvm mem msz)
  (-> (vectorof integer?) integer? icvm?)
  (let ((m (build-vector msz (const 0))))
    (vector-copy! m 0 mem)
    (icvm m 0 #f #f (build-imap) (const #f) (const #f))))

(define/contract (icvm-load is msz)
  (-> (listof string?) integer? icvm?)
  (make-icvm
    (list->vector
      (flatten
        (map (lambda (i)
                (map string->number (string-split i ","))) is)))
    msz))

(define (icvm-step! vm)
  (define (fetch-args vm pc a)
    (vector->list (vector-copy (icvm-mem vm) (add1 pc) (+ (add1 pc) a))))

  (define (modenum->mode mn)
    (case mn
      ((0) 'pos)
      ((1) 'imm)
      (else (error "bad mode ~a~n" mn))))

  (define (decode vm in)
    (let* ((pm2 (modulo (floor (/ in 10000)) 10))
           (pm1 (modulo (floor (/ in 1000)) 10))
           (pm0 (modulo (floor (/ in 100)) 10))
           (in (vector-ref (icvm-imap vm) (modulo in 100))))
      (values in (list (modenum->mode pm0)
                       (modenum->mode pm1)
                       (modenum->mode pm2)))))

  (define (dispatch! vm pc in pmodes)
    (let* ((a (sub1 (procedure-arity in)))
           (argvals (fetch-args vm pc a))
           (args (map cons argvals (take pmodes (length argvals)))))
      (when (icvm-trace vm)
        (printf "~a ~a ~a~n" (icvm-pc vm) in args))
      (set-icvm-pc! vm (+ pc a 1))
      (apply in vm args)))

  (let ((pc (icvm-pc vm)))
    (if (or (< pc 0)
            (>= pc (vector-length (icvm-mem vm))))
        (set-icvm-halted! vm #t)
        (let-values (((in pmodes) (decode vm (icvm-memref vm pc))))
          (dispatch! vm pc in pmodes))))
  vm)

(define (icvm-run! vm)
  (do () ((icvm-halted vm))
    (icvm-step! vm)))

(define (with-fixed-io vm ins p)
  (let ((outs (list))
        (vm (icvm-copy vm)))
    (set-icvm-inproc! vm
      (lambda ()
        (if (null? ins)
            #f
            (let ((n (car ins)))
              (set! ins (cdr ins))
              n))))
    (set-icvm-outproc! vm
      (lambda (v)
        (set! outs (cons v outs))))
    (icvm-run! vm)
    (p (reverse outs))))
