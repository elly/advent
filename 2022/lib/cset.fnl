; lib/cset - counted sets (aka multisets)

(fn add [t k]
  (when k
    (tset t k (+ (or (. t k) 0) 1))))

(fn del [t k]
  (when k
    (let [r (. t k)]
      (if (= r 1)
          (tset t k nil)
          (tset t k (- r 1))))))

(fn hasdupes? [t]
  (var r false)
  (each [_ v (pairs t)]
    (when (> v 1)
          (set r true)))
  r)

(fn make [x] {})

(fn check []
  (let [s (make)]
    (add s :a)
    (add s :b)
    (assert (not (hasdupes? s)))
    (add s :a)
    (assert (hasdupes? s))
    (del s :a)
    (assert (not (hasdupes? s)))))

{
  : add
  : check
  : del
  : hasdupes?
  : make
}
