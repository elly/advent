; 2022/6

(local cset (require "../lib/cset"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn read [lines] (. lines 1))

(fn start-idx [s n]
  (let [ps (str.explode s) win (cset.make)]
    (var r false)
    (for [i 1 n 1]
      (cset.add win (. ps i)))
    (for [i (+ n 1) (length ps) 1
          &until (not (cset.hasdupes? win))]
      (cset.add win (. ps i))
      (cset.del win (. ps (- i n)))
      (set r i))
    r))

(fn solve-a [x] (start-idx x 4))
(fn solve-b [x] (start-idx x 14))

{
  : read
  : solve-a
  : solve-b
}
