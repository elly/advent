; 2022/0
; I usually invent a "day 0" each year, during which I set up the scaffolding
; I need to do the problems and make sure I know enough of the language to be
; effective. This year, my day 0 takes a single string, and:
;   part a returns how many left parens are in it
;   part b returns the depth of the deepest element in the string, interpreted
;     as a lisp-style tree
; To do part a, it produces a frequency table which the solution indexes into,
; and to do part b, it produces a depth table which the solution finds a max
; depth from. Simple :)

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

(fn check []
  (let [fs (freqs "(a(b)(c)d)")]
    (assert (= (. fs :left) 3))
    (assert (= (. fs :right) 3)))
  (let [ds (depths "(a(b)(c)d)")]
    (assert (= 1 (. ds :a)))
    (assert (= 2 (. ds :b)))
    (assert (= 2 (. ds :c)))
    (assert (= 1 (. ds :d)))))

{
  :read read
  :check check
  :solve-a (fn [t] (. t :freqs :left))
  :solve-b (fn [t] (deepest (. t :depths)))
}
