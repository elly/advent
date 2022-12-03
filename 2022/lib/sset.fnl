; lib/set - set library

(fn any [s]
  (var r nil)
  (each [k _ (pairs s)]
    (if (not r)
        (set r k)))
  r)

(fn eq [s0 s1]
  (var r true)
  (each [k _ (pairs s0)]
    (if (not (. s1 k))
        (set r false)))
  (each [k _ (pairs s1)]
    (if (not (. s0 k))
        (set r false)))
  r)

(fn intersect [sets]
  (var r {})
  (each [k _ (pairs (. sets 1))]
    (tset r k true))
  (for [n 2 (length sets) 1]
    (each [k _ (pairs r)]
      (if (not (. sets n k))
          (tset r k nil))))
  r)
 
(fn items [s]
  (var r [])
  (each [k _ (pairs s)]
    (table.insert r k))
  (table.sort r)
  r)

(fn of [t]
  (var r {})
  (each [_ v (ipairs t)]
    (assert (= (type v) "string"))
    (tset r v true))
  r)

(fn check []
  (let [s0 (of [:a :b :c])
        s1 (of [:b :c :d])
        s2 (of [:c :d :e])]
    (assert (eq (of [:b :c]) (intersect [s0 s1])))
    (assert (eq (of [:c]) (intersect [s0 s2])))))

{
  : any
  : check
  : eq
  : intersect
  : items
  : of
}
