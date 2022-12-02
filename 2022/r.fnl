#!/usr/bin/fennel5.4

; This is the runner for my Fennel Advent solutions. You run it like this:
;   ./r.fnl <day number> <input type>
; where the input type is generally either "real" or a test case starting with
; "t", like "t0".
;
; Each solution is a module that exports at least:
;   solve-a
;   solve-b
; and maybe also:
;   read
;   check
;
; The intended design is that:
; read    is given the input lines (as a table) and returns whatever data
;         structure is needed to solve the problem
; solve-a takes the data structure from read and solves part a
; solve-b takes the data structure from read and solves part b
; check   runs unit tests - it has no return value, so it should either finish
;         or crash the program with assert
; If no read is provided, the default one just returns the input as an array
; of lines. If no check is provided, the default one does nothing.

(local fennel (require :fennel))
(global pretty (fn [x] (print (fennel.view x))))

(fn check-lib [name]
  (let [m (require (.. "lib/" name))]
    ((. m :check))))

(fn check-libs []
  (check-lib "points"))

(fn default-read [v] v)
(fn default-check [] true)

(fn fp [args s] (.. (. args 1) "/" (. args 2) "." s))

(fn lines-from-file [name dflt]
  (let [fin (io.open name "r")]
    (if
      fin (icollect [line (fin:lines)] line)
      dflt dflt
      (assert false (.. "?f " name)))))

(fn verify-output [n got expected]
  (let [got (or got "(nil)")]
    (if
      (not expected) (print (.. n ": " got))
      (not (= (.. "" got) (.. "" expected)))
        (print (.. n ": " got " (bad " expected ")"))
      (print (.. n ": " got " (ok)")))))

(fn main [args]
  (let [mod (require (.. (. args 1) "/s"))
        input (lines-from-file (fp args "in"))
        output (lines-from-file (fp args "out") [])]
    (let [solve-a (. mod :solve-a)
          solve-b (. mod :solve-b)
          read (or (. mod :read) default-read)
          check (or (. mod :check) default-check)]
      (check)
      (let [p (read input)
            a (solve-a p)
            b (solve-b p)]
        (verify-output :a a (. output 1))
        (verify-output :b b (. output 2))))))

(check-libs)
(if (>= (length arg) 2)
    (main arg))
