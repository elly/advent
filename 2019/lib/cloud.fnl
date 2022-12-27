; lib/cloud - a cloud of points with a value at each point

(fn add! [c [x y z] v]
  (local v (or v true))
  (fn ensure [t k]
    (when (not (. t k))
          (tset t k {})))

  (ensure c.pts x)
  (ensure (. c.pts x) y)
  (when (= (. c.pts x y z) nil)
        (tset c :card (+ 1 c.card)))
  (tset (. c.pts x y) z v))

(fn all [c]
  (var r [])
  (each [xk xv (pairs c.pts)]
    (each [yk yv (pairs xv)]
      (each [zk zv (pairs yv)]
        (table.insert r [xk yk zk]))))
  r)

(fn get [c [x y z]]
  (?. c.pts x y z))

(fn has [c [x y z]]
  (not (= nil (?. c.pts x y z))))

(fn make [] {
  :card 0
  :pts {}
})

(fn intersect [c0 c1 f]
  (local f (or f (fn [_ _] true)))
  (var n (make))
  (each [_ p (ipairs (all c0))]
    (when (has c1 p)
          (add! n p (f (get c0 p) (get c1 p)))))
  n)

(fn of [pts d]
  (local d (or d true))
  (var n (make))
  (each [_ p (ipairs pts)]
    (add! n p true))
  n)

(fn check []
  (let [p (of [[1 2 3] [4 5 6]])
        q (of [[4 5 6] [7 8 9]])]
    (assert (= 2 p.card))
    (assert (= 2 q.card))
    (assert (= 1 (. (intersect p q) :card)))))

{
  : add!
  : all
  : check
  : get
  : has
  : intersect
  : make
  : of
}
