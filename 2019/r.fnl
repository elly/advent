#!/usr/bin/fennel5.4
; Advent 2019 runner

(local fennel (require :fennel))

(global pretty (fn [x] (print (fennel.view x)) x))

(fn check-lib [name]
  (let [m (require (.. "lib/" name))]
    (m.check)))

(fn check-libs [])

(fn lines-from-file [name default]
  (let [fin (io.open name "r")]
    (if
      fin         (icollect [line (fin:lines)] line)
      default     default
                  (error "missing input file"))))

(fn verify-output [name got expected]
  (let [sg (.. "" got)]
    (if (not expected)  (print (.. name ": " sg))
        (= sg expected) (print (.. name ": " sg " (ok)"))
                        (print (.. name ": " sg " (expected " expected ")")))))

(fn main [[day ctype]]
  (let [mod (require (.. day "/s"))
        input (lines-from-file (.. day "/" ctype ".in"))
        output (lines-from-file (.. day "/" ctype ".out") [])]
    (when mod.check
      (mod.check))
    (let [p1 (if mod.read (mod.read input) input)
          p2 (if mod.prep (mod.prep p1) p1)]
      (when (and mod.read mod.debug)
        (pretty p1))
      (when (and mod.prep mod.debug)
        (pretty p2))
      (when mod.solve-a
        (local sa (mod.solve-a p2))
        (verify-output :a sa (. output 1)))
      (when mod.solve-b
        (local sb (mod.solve-b p2))
        (verify-output :b sb (. output 2))))))

(check-libs)
(if (>= (length arg) 2)
    (main arg))
