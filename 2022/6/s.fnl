; 2022/6

(local sset (require "../lib/sset"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn read [lines] (. lines 1))

(fn start? [cs n]
  (let [t (sset.of (tbl.take cs n))]
    (= n (sset.size t))))

(fn start-idx [s n]
  (let [ps (str.explode s)]
    (var r false)
    (for [i 0 (- (length ps) 1) 1 &until r]
      (when (start? (tbl.drop ps i) n)
            (set r i)))
    (+ r n)))

(fn check []
  (assert (start? [:a :b :c :d]))
  (assert (not (start? [:a :b :c :a]))))

(fn solve-a [x] (start-idx x 4))
(fn solve-b [x] (start-idx x 14))

{
  : read
  : solve-a
  : solve-b
}
