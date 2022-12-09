; lib/points - 3d point sets

(fn add [s p v]
  (let [x (. p 1)
        y (. p 2)
        z (. p 3)]
  (if (not (. s x))
      (tset s x {}))
  (if (not (. s x y))
      (tset s x y {}))
  (tset s x y z v)
  s))

(fn all [s f]
  (each [xk xv (pairs s)]
    (each [yk yv (pairs xv)]
      (each [zk zv (pairs yv)]
        (f [xk yk zk] zv)))))

(fn get [s p]
  (?. s (. p 1) (. p 2) (. p 3)))

(fn make [] {})

(fn check []
  (let [t (make)
        p1 [1 2 3]
        p2 [3 1 2]
        p3 [2 3 1]]
    (add t p1 1)
    (add t p2 2)
    (add t p3 3)
    (assert (= 1 (get t p1)))
    (assert (= 2 (get t p2)))
    (assert (= 3 (get t p3)))
    (assert (= nil (get t [1 1 3])))))

{
  : add
  : all
  : check
  : get
  : make
}
