; lib/list - list primitives

(fn maximize [list f]
  (var maxkey nil)
  (var maxval nil)
  (each [_ k (ipairs list)]
    (let [r (f k)]
      (when (or (not maxval) (> r maxval))
        (set maxkey k)
        (set maxval r))))
  (values maxkey maxval))

(fn permutations [list n r]
  (fn swap [i j]
    (local e (. list j))
    (tset list j (. list i))
    (tset list i e))
  (var n (or n (# list)))
  (var r (or r []))
  ; base case: list of length 0 has no permutations
  ; recursive case: for each element, erase it, generate all permutations of
  ;                 the sublist not including it, then tack it onto the end of
  ;                 those sublists.
  ; this approach is ripped off from Programming in Lua 9.3
  (if (= n 0)
      (table.insert r (icollect [_ v (ipairs list)] v))
      (for [i 1 n 1]
        (swap i n)
        (permutations list (- n 1) r)
        (swap i n)))

  r)

(fn check [])

{
  : check
  : maximize
  : permutations
}
