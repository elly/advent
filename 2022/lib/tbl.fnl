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

(fn drop [t n]
  (var r {})
  (for [i (+ n 1) (length t) 1]
    (table.insert r (. t i)))
  r)

(fn filter [t f]
  (var r [])
  (each [_ v (ipairs t)]
    (if (f v)
        (table.insert r v)))
  r)

(fn find [t f]
  (var r nil)
  (each [_ v (ipairs t) &until r]
    (if (f v)
        (set r v)))
  r)

(fn fold [t v f]
  (var x v)
  (each [_ v (ipairs t)]
    (set x (f x v)))
  x)

(fn group [t n]
  (var r [])
  (var cr [])
  (var crn 0)
  (each [_ v (ipairs t)]
    (table.insert cr v)
    (set crn (+ crn 1))
    (if (= crn n)
        (do
          (table.insert r cr)
          (set cr [])
          (set crn 0))))
  (if (> crn 0)
      (table.insert r cr))
  r)

(fn indexf [t f]
  (var r nil)
  (each [k v (ipairs t) &until (not (= r nil))]
    (if (f v)
        (set r k)))
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

(fn pop [t]
  (var r (acopy t))
  (table.remove r (length r))
  r)

(fn prod [t]
  (accumulate [s 1 _ v (pairs t)]
    (* s v)))

(fn push [t v]
  (var r (acopy t))
  (table.insert r v)
  r)

(fn reverse [t]
  (var r [])
  (for [i (length t) 1 -1]
    (table.insert r (. t i)))
  r)

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

(fn check []
  (assert (aeq [3 4] (drop [1 2 3 4] 2)))
  (assert (aeq [1 2] (take [1 2 3 4] 2)))
  (let [g (group [:a :b :c :d :e :f :g :h] 2)]
    (assert (aeq [:a :b] (. g 1)))
    (assert (aeq [:g :h] (. g 4))))
  (assert (aeq [3 2 1] (reverse [1 2 3]))))

{
  : aeq
  : acopy
  : check
  : drop
  : find
  : filter
  : fold
  : group
  : indexf
  : map
  : maximize
  : maxval
  : pop
  : prod
  : push
  : reverse
  : sorted
  : splitby
  : sum
  : take
  : update
}
