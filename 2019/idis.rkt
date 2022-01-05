#lang scheme

(require srfi/1)

(define (inum->itype in)
  (case in
    ((1)  '(add 3))
    ((2)  '(mul 3))
    ((3)  '(in 1))
    ((4)  '(out 1))
    ((5)  '(jt 2))
    ((6)  '(jf 2))
    ((7)  '(lt 3))
    ((8)  '(eq 3))
    ((9)  '(arb 1))
    ((99) '(hlt 0))
    (else #f)))

(define itype-name first)
(define itype-arity second)

(define instruction-type first)
(define instruction-args second)
(define instruction-nextpcs third)
(define (instruction-length in) (add1 (length (instruction-args in))))

(define (modenum->mode mn)
  (case mn
    ((0) 'pos)
    ((1) 'imm)
    ((2) 'rel)
    (else (error "bad mode ~a~n" mn))))

(define (decode-args prog pc pms)
  (let loop ((pc (add1 pc)) (pms pms) (as (list)))
    (if (null? pms)
        (reverse as)
        (loop (add1 pc)
              (cdr pms)
              (cons (cons (car pms) (vector-ref prog pc)) as)))))

(define (decode in)
  (let ((pm2 (modulo (floor (/ in 10000)) 10))
        (pm1 (modulo (floor (/ in 1000)) 10))
        (pm0 (modulo (floor (/ in 100)) 10))
        (inn (modulo in 100)))
    (if
      (not (inum->itype inn))
      #f
      (let ((it (inum->itype inn)))
        (cons it
              (take (list (modenum->mode pm0)
                          (modenum->mode pm1)
                          (modenum->mode pm2))
                    (itype-arity it)))))))

(define (add-nextpcs prog pc in)
  (define (const-value a)
    (and (equal? (car a) 'imm)
         (cdr a)))

  (define (nextpc)
    (+ pc (instruction-length in)))

  (define (nextpcs-for-jc pred)
    (let ((args (instruction-args in)))
      (let ((cp (const-value (first args)))
            (ct (const-value (second args))))
        (cond
          ((and cp ct (pred cp)) (list ct))
          ((and cp ct)           (list (nextpc)))
          (cp                    (list))          ; XXX
          (ct                    (list ct (nextpc)))
          (else                  (begin (printf "ct ~a ~a~n" pc in)
                                        (list (nextpc))))))))

  (define (nextpcs-for prog pc in)
    (case (itype-name (instruction-type in))
      ((jt) (nextpcs-for-jc (lambda (e) (not (= e 0)))))
      ((jf) (nextpcs-for-jc (lambda (e) (= e 0))))
      ((hlt) (list))
      (else (list (nextpc)))))

  (append in (list (nextpcs-for prog pc in))))

(define (disas1 prog pc)
  (let ((in (decode (vector-ref prog pc))))
    (if in
        (let ((ins (car in)) (pms (cdr in)))
          (let ((args (decode-args prog pc pms)))
            (add-nextpcs prog pc (list ins args))))
        #f)))

(define (disas prog)
  (let loop ((mm (list)) (uv (list 0)))
    (cond
      ((null? uv)          mm)
      ((assoc (car uv) mm) (loop mm (cdr uv)))
      (else
        (let ((i (disas1 prog (car uv))))
          (loop (cons (cons (car uv) i) mm)
                (append uv (instruction-nextpcs i))))))))

(define (parse in)
  (list->vector (map string->number (string-split in ","))))

(define (render prog di) 0)

(let* ((prog (parse (read-line)))
       (di (disas prog)))
  (render prog di))
