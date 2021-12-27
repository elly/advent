#lang racket

; This is the main driver program for my Racket advent solutions. It parses
; the command line to figure out which day is being run and what input file(s)
; to use with it, runs the solution, then checks the output if there are known
; correct outputs available.
;
; Specifically, the command line always contains a year/day pair, and then
; a verb. The year/day pair refer to a directory underneath the current one.
; For example:
;   adv.rkt 2015/01 check   <---- run on supplied test inputs & validate
;   adv.rkt 2015/01 solve   <---- run on real input (& validate if able)
;   adv.rkt 2015/01 test    <---- run unit tests

(require "day.rkt")

(define (lines-from-file path)
  (let ((f (open-input-file path)))
    (let loop ((ls '()))
      (let ((l (read-line f)))
        (if (eof-object? l)
            (reverse ls)
            (loop (cons l ls)))))))

(define (datums-from-file p)
  (map (compose read open-input-string) (lines-from-file p)))

(define (parse-args cml)
  (if
    (< (vector-length cml) 2)
    (begin
      (printf "Usage: ~a year/day verb~n" (find-system-path 'run-file)))
    (values (string->path (vector-ref cml 0))
            (vector-ref cml 1))))

(define (build-day dp spec)
  (let* ((pc (explode-path dp))
         (y (string->number (path-element->string (first pc))))
         (d (string->number (path-element->string (second pc)))))
    (day y d (first spec) (second spec) (third spec) (fourth spec)
             (fifth spec) dp)))

(define/contract (load-day dirpath)
  (-> path? (or/c day? #f))
  (let ((mp (build-path 'same dirpath "s.rkt")))
    (and (file-exists? mp)
         (build-day dirpath (dynamic-require mp 'today)))))

(define/contract (paths-for-check d)
  (-> day? (listof (cons/c path? path?)))

  (define (looks-like-test-dir? dp)
    (let ((s (path-element->string dp)))
      (string-prefix? s "t")))

  (define (paths-for-test-dir pre dp)
    (and (looks-like-test-dir? dp)
         (cons (build-path pre dp "in")
               (build-path pre dp "out"))))

  (let ((ins (directory-list (day-path d))))
    (filter-map (curry paths-for-test-dir (day-path d)) ins)))

(define/contract (path-for-solve d)
  (-> day? (cons/c path? path?))

  (cons (build-path (day-path d) "in")
        (build-path (day-path d) "out")))

(define (report-result d ps p r)
  (printf "~a ~a: ~a~n" (car ps) p r))

(define (compare-results d c pa pb)
  (or (not c)
      (and (= (length c) 2)
           (equal? (first c) pa)
           (equal? (second c) pb))))

(define (run-on-path-pair d ps)
  (let ((in (lines-from-file (car ps)))
        (out (and (file-exists? (cdr ps)) (datums-from-file (cdr ps)))))
    (let* ((i ((day-parse d) in))
           (e ((day-extract d) i))
           (pa ((day-solve-a d) e))
           (pb ((day-solve-b d) e)))
      (report-result d ps "a" pa)
      (report-result d ps "b" pb)
      (compare-results d out pa pb))))

(define (check d)
  (let ((ps (paths-for-check d)))
    (when (not (andmap (curry run-on-path-pair d) ps))
          (error "check mismatch"))))

(define (solve d)
  (when (not (run-on-path-pair d (path-for-solve d)))
        (error "solve mismatch")))

(define (test day)
  ((day-test day)))

(define (main args)
  (let-values ([(path verb) (parse-args args)])
    (let ((day (load-day path)))
      (cond
        [(not day) (error "load failed:" path)]
        [(equal? verb "test") (test day)]
        [(equal? verb "check") (check day)]
        [(equal? verb "solve") (solve day)]
        [else (error "Unknown verb, try: check solve test")]))))

(main (current-command-line-arguments))
