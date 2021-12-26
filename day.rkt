#lang racket

(provide day day?
         day-year day-number
         day-parse day-extract day-solve-a day-solve-b
         day-unit-test)

(struct day (year number parse extract solve-a solve-b unit-test) #:transparent)
