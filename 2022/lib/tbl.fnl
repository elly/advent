; lib/tbl: table library

(fn update [t k f d]
  (tset t k (f (or (. t k) d))))

(fn maximize [t f]
  (var mv 0)
  (var mk nil)
  (var f (or f (fn [x] x)))
  (each [k v (pairs t)]
    (let [nv (f v)]
      (if (> nv mv)
          (do
            (set mv nv)
            (set mk k)))))
  mk)

(fn maxval [t f]
  (. t (maximize t f)))

{
  :maximize maximize
  :maxval maxval
  :update update
}
