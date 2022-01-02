#lang racket

(provide today)
(require "../intcode.rkt"
         "../../lib/geom.rkt")

(struct paintbot (vm pos dir pps movef) #:mutable)

(define (make-paintbot vm start-color)
  (define (turn! pb d)
    (let ((od (paintbot-dir pb))
          (up (point 0 -1 0))
          (right (point 1 0 0))
          (down (point 0 1 0))
          (left (point -1 0 0)))
      (set-paintbot-dir! pb
        (cond
          ((and (= d 0) (equal? od up))    left)
          ((and (= d 0) (equal? od left))  down)
          ((and (= d 0) (equal? od down))  right)
          ((and (= d 0) (equal? od right)) up)
          ((and (= d 1) (equal? od up))    right)
          ((and (= d 1) (equal? od left))  up)
          ((and (= d 1) (equal? od down))  left)
          ((and (= d 1) (equal? od right)) down)))))

  (define (move! pb d)
    (turn! pb d)
    (set-paintbot-pos! pb (point+ (paintbot-pos pb) (paintbot-dir pb))))

  (define (paint! pb o)
    (hash-set! (paintbot-pps pb) (paintbot-pos pb) o))

  (let ((pb (paintbot vm *origin-point* (point 0 -1 0)
                      (make-hash (list (cons *origin-point* start-color))) #f)))
    (set-icvm-inproc! vm
      (lambda () (hash-ref (paintbot-pps pb) (paintbot-pos pb) 0)))
    (set-icvm-outproc! vm
      (lambda (o)
        (if (paintbot-movef pb)
            (move! pb o)
            (paint! pb o))
        (set-paintbot-movef! pb (not (paintbot-movef pb)))))
    pb))

(define (run-paintbot vm sc)
  (let ((pb (make-paintbot (icvm-copy vm) sc)))
    (icvm-run! (paintbot-vm pb))
    (paintbot-pps pb)))

(define (raster ps)
  (for ((y 8))
    (for ((x 60))
      (printf "~a"
        (if (= (hash-ref ps (point x y 0) 0) 0) #\space #\#)))
    (printf "~n"))
  (length (hash-keys ps)))

(define today (list (curryr icvm-load 8192)
                    identity
                    (compose set-count
                             list->set
                             hash-keys
                             (curryr run-paintbot 0))
                    (compose raster
                             (curryr run-paintbot 1))))
