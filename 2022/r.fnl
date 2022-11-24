#!/usr/bin/env fennel

; This is the runner for my Fennel Advent solutions. You run it like this:
;   ./r.fnl <day number> <input type>
; where the input type is generally either "real" or a test case starting with
; "t", like "t0".
;
; Each solution is a module that exports at least:
;   solve-a
;   solve-b
; and maybe also:
;   parse
;   extract
;   check

(let [m (require (.. (. arg 1) "/s"))]
  (let [solve-a (. m :solve-a)]
       [solve-b (. m :solve-b)]
       [parse (or (. m :parse) default-parse)]
       [extract (or (. m :extract) default-extract)]
       [check (or (. m :check) default-check)]
    ))
