#lang racket

(provide today)

(struct image (layers dim))

(define (parse ls)

  (define (parse-layers px dm)
    (if (null? px)
        (list)
        (let-values (((layer rest) (split-at px (* (car dm) (cdr dm)))))
          (cons layer (parse-layers rest dm)))))

  (define (parse-dim ls)
    (if (= (length ls) 2)
        (let ((lp (string-split (second ls) " ")))
          (cons (string->number (first lp))
                (string->number (second lp))))
        (cons 6 25)))

  (let ((dim (parse-dim ls)))
    (image (parse-layers (map (compose string->number string)
                              (string->list (first ls)))
                         dim)
           dim)))

(define (solve-a im)
  (let ((bp (argmin (lambda (l) (count (curry = 0) l)) (image-layers im))))
    (* (count (curry = 1) bp)
       (count (curry = 2) bp))))

(define (flatten im)
  (define (flatten-pixel i)
    (ormap
      (lambda (v) (and (or (= v 0) (= v 1)) v))
      (map (curryr list-ref i)
           (image-layers im))))

  (image
    (list
      (build-list (length (first (image-layers im)))
                  flatten-pixel))
    (image-dim im)))

(define (raster im)
  (define (pixel-for y x)
    (list-ref (first (image-layers im))
              (+ (* y (cdr (image-dim im))) x)))

  (for ((y (car (image-dim im))))
    (for ((x (cdr (image-dim im))))
      (printf "~a" (if (= (pixel-for y x) 1) "#" " ")))
    (printf "~n")))

(define (solve-b im)
  (let ((im (flatten im)))
    (raster im)
    0))

(define today (list parse identity solve-a solve-b))
