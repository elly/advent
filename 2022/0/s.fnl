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

(local func (require "../lib/func"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn add1 [x] (+ x 1))

(fn freqs [t]
  (let [r {}]
    (each [c (str.chars t)]
      (tbl.update r c add1 0))
    r))

(fn depths [t]
  (var r {})
  (var d 0)
  (each [c (str.chars t)]
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

(fn check []
  (let [fs (freqs "(a(b)(c)d)")]
    (assert (= (. fs "(") 3))
    (assert (= (. fs ")") 3)))
  (let [ds (depths "(a(b)(c)d)")]
    (assert (= 1 (. ds :a)))
    (assert (= 2 (. ds :b)))
    (assert (= 2 (. ds :c)))
    (assert (= 1 (. ds :d)))))

{
  : read
  : check
  :solve-a (fn [t] (. t :freqs "("))
  :solve-b (fn [t] (tbl.maxval (. t :depths)))
}
