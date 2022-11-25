(fn freqs [t]
  (let [r { :left 0 :right 0 }]
    (each [c (t:gmatch ".")]
      (if
        (= c "(") (tset r :left (+ (. r :left) 1))
        (= c ")") (tset r :right (+ (. r :right) 1))))
    r))

(fn depths [t]
  (var r {})
  (var d 0)
  (each [c (t:gmatch ".")]
    (if
      (= c "(") (set d (+ d 1))
      (= c ")") (set d (- d 1))
      (tset r c d)))
  r)

(fn read [lines]
  (let [t (. lines 1)]
    {
      :freqs (freqs t)
      :depths (depths t)
    }))

(fn deepest [depths]
  (var md 0)
  (each [k d (pairs depths)]
    (if
      (> d md)
      (set md d)))
  md)

{
  :read read
  :solve-a (fn [t] (. t :freqs :left))
  :solve-b (fn [t] (deepest (. t :depths)))
}
