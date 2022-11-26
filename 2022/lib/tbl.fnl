(fn copy [t]
  (if (= (type t) "table")
      (let [r {}]
        (each [k v (pairs t)]
          (tset r (copy k) (copy v)))
        r)
      t))

(fn update [t k f d]
  (let [n (copy t)]
    (tset n k
      (f (or (. t k) d)))
    n))

{
  :copy copy
  :update update
}
