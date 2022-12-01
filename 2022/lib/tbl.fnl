; lib/tbl: table library

(fn aeq [t0 t1]
  (var r true)
  (each [k v (ipairs t0)]
    (if (not (= v (. t1 k)))
        (set r false)))
  r)

(fn acopy [t]
  (var r {})
  (each [_ v (ipairs t)]
    (table.insert r v))
  r)

(fn map [t f]
  (var r {})
  (each [k v (pairs t)]
    (tset r k (f v)))
  r)

(fn maximize [t f]
  (var mv 0)
  (var mk nil)
  (var f (or f #$1))
  (each [k v (pairs t)]
    (let [nv (f v)]
      (if (> nv mv)
          (do
            (set mv nv)
            (set mk k)))))
  mk)

(fn maxval [t f]
  (. t (maximize t f)))

(fn sorted [t f]
  (var r (acopy t))
  (table.sort r f)
  r)

(fn splitby [t f]
  (var r {})
  (var c {})
  (each [_ v (pairs t)]
    (if (f v)
        (do
          (table.insert r c)
          (set c {}))
        (table.insert c v)))
  (if (> (# c) 0)
      (table.insert r c))
  r)

(fn sum [t]
  (accumulate [s 0 _ v (pairs t)]
    (+ s v)))

(fn take [t n]
  (var r {})
  (for [i 1 n 1]
    (table.insert r (. t i)))
  r)

(fn update [t k f d]
  (tset t k (f (or (. t k) d))))

{
  : aeq
  : acopy
  : map
  : maximize
  : maxval
  : sorted
  : splitby
  : sum
  : take
  : update
}
