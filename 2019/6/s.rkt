#lang racket

(provide today)
(require "../../lib/list.rkt")

(define/contract (parse ls)
  (-> (listof string?) (hash/c symbol? (listof symbol?)))
  (foldl
    (lambda (l m)
      (let* ((ps (string-split l ")"))
             (lhs (string->symbol (first ps)))
             (rhs (string->symbol (second ps))))
        (hash-update m lhs (curry cons rhs) list)))
    (hash)
    ls))

(define (build-otree-from os r)
  (let ((ics (hash-ref os r (list))))
    (if (null? ics)
        (list r)
        (cons r (map (curry build-otree-from os) ics)))))

(define (find-root os)
  (let ((ls (list->set (hash-keys os)))
        (rs (list->set (flatten (hash-values os)))))
    (set-first (set-subtract ls rs))))

(define (build-otree os)
  (build-otree-from os (find-root os)))

(define (depths t)
  (define (depths-h t d)
    (if (symbol? t)
        (list d)
        (cons d (map (curryr depths-h (add1 d)) (cdr t)))))
  (depths-h t 0))

(define (path-to t n)
  (cond
    ((equal? (first t) n) (list n))
    ((= (length t) 1) #f)
    (else
      (let ((cps (filter-map (curryr path-to n) (cdr t))))
        (if (null? cps)
            #f
            (cons (car t) (first cps)))))))

(define (san-path t)
  (let ((sp (path-to t 'SAN))
        (yp (path-to t 'YOU)))
    (let-values (((sr yr) (drop-common-prefix sp yp)))
      (- (+ (length sr) (length yr)) 2))))

(define today (list parse build-otree (compose sum flatten depths) san-path))
