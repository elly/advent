#lang racket

(provide day day?
         day-year day-number
         day-parse day-extract day-solve-a day-solve-b
         day-test

         day-path)

(struct day (year number parse extract solve-a solve-b test path) #:transparent)
