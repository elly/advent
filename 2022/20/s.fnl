; 2022/20: Grove Positioning System

(local tbl (require :../lib/tbl))

(fn check [] true)

(fn read [lines] (tbl.map lines tonumber))

(fn tolinked [ns]
  (var h { :v (. ns 1 ) })
  (tset h :n h)
  (tset h :p h)
  (for [i 2 (# ns) 1]
    (var n { :v (. ns i) })
    (tset n :p h.p)
    (tset n :n h)
    (tset h.p :n n)
    (tset h :p n))
  (values h (# ns)))

(fn linkfind [linked v]
  (var h linked)
  (var found (if (= linked.v v) linked nil))
  (while (and (not (= h.n linked)) (not found))
         (set h h.n)
         (if (= h.v v)
             (set found h)))
  found)

(fn toptr [t] (string.gsub (tostring t) "table: " ""))

(fn unlink [t]
  (tset t.p :n t.n)
  (tset t.n :p t.p)
  (tset t :p nil)
  (tset t :n nil))

(fn link [after t]
  (tset t :n after.n)
  (tset t :p after)
  (tset after.n :p t)
  (tset after :n t))

(fn abs [x] (if (< x 0) (* -1 x) x))

(fn walk [node n]
  (local k (if (< n 0) :p :n))
  (var t node)
  (var i 0)
  (while (not (= i (abs n)))
    (set i (+ i 1))
    (set t (. t k)))
  t)

(fn dump [linked]
  (fn prn [node]
    (print (.. (toptr node.p)
           " <- "
           (toptr node)
           " [" node.v "] -> "
           (toptr node.n))))
  (print (.. "dumping " (toptr linked)))
  (var h linked.n)
  (prn linked)
  (while (not (= h linked))
    (prn h)
    (set h h.n))
  linked)

(fn move [linked lc t d]
  (var p t.p)
  (unlink t)
  (var np (walk p (% d (- lc 1))))
  (link np t))

(fn flatten [linked]
  (var r [linked])
  (var h linked)
  (while (not (= h.n linked))
         (set h h.n)
         (table.insert r h))
  r)

(fn mix [linked lc rounds]
  (local rounds (or rounds 1))
  (local items (flatten linked))
  (for [i 1 rounds 1]
    (each [_ n (ipairs items)]
      (move linked lc n n.v)))
  linked)

(fn extract [h]
  (+ (. (walk h 1000) :v)
     (. (walk h 2000) :v)
     (. (walk h 3000) :v)))

; 10144 too high
; 8427 too high
(fn solve-a [numbers]
  (-> numbers
      tolinked
      mix
      (linkfind 0)
      extract))

(fn solve-b [numbers]
  (let [(l n) (tolinked (tbl.map numbers #(* $1 811589153)))]
    (mix l n 10)
    (extract (linkfind l 0))))

(fn check []
  (var q (tolinked [1 2 3]))
  (assert (linkfind q 1))
  (assert (linkfind q 3))
  (assert (not (linkfind q 4)))

  (assert-eq 1 (. (walk q 0) :v))
  (assert-eq 2 (. (walk q 1) :v))
  (assert-eq 3 (. (walk q 2) :v))
  (assert-eq 1 (. (walk q 3) :v))
  (assert-eq 3 (. (walk q -1) :v))
  (assert-eq 2 (. (walk q -2) :v))

  (let [z (linkfind q 1)
        t (linkfind q 2)]
    (unlink z)
    (link t z)
    (assert (linkfind z 1))
    (assert (linkfind z 2))
    (assert (linkfind z 3))
    (assert (linkfind t 1))
    (assert (linkfind t 2))
    (assert (linkfind t 3)))

  (fn t [d p n]
    (var l (tolinked [:a :b :c :d :e]))
    (move l 5 (linkfind l :a) d)
    (assert-eq (. (linkfind l :a) :p :v) p)
    (assert-eq (. (linkfind l :a) :n :v) n))

  (t 0 :e :b)
  (t 1 :b :c)
  (t 2 :c :d)
  (t 3 :d :e)
  (t 4 :e :b)

  (t -1 :d :e)
  (t -2 :c :d)
  (t -3 :b :c)
  (t -4 :e :b)
)

{
  : check
  : read
  : solve-a
  : solve-b
}
