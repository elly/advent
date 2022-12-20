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
  h)

(fn linkfind [linked v]
  (var h linked)
  (var found (if (= linked.v v) linked nil))
  (while (and (not (= h.n linked)) (not found))
         (set h h.n)
         (if (= h.v v)
             (set found h)))
  found)

(fn linkcheck [linked]
  (fn checknode [n]
    (assert n.n)
    (assert n.p)
    (assert (= n.p.n n))
    (assert (= n.n.p n)))

  (checknode linked)
  (var h linked.n)
  (while (not (= h linked))
    (checknode h)
    (set h h.n)))

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
  (var t node)
  (local k (if (< n 0) :p :n))
  (for [i 1 (abs n) 1]
    (set t (. t k)))
  t)

(fn dump [linked]
  (fn prn [node]
    (print (.. (toptr node.p)
           " <- "
           (toptr node)
           " [" node.v "] -> "
           (toptr node.n))))
  (var h linked.n)
  (prn linked)
  (while (not (= h linked))
    (prn h)
    (set h h.n))
  linked)

(fn move [linked v]
  (linkcheck linked)
  (local t (linkfind linked v))
  (local p (walk t.p v))
  (unlink t)
  (link p t)
  (linkcheck linked))

(fn flatten [linked]
  (var r [linked.v])
  (var h linked)
  (while (not (= h.n linked))
         (set h h.n)
         (table.insert r h.v))
  r)

(fn mix [linked]
  (each [_ v (ipairs (flatten linked))]
    (move linked v))
  linked)

; 10144 too high
; 8427 too high
(fn solve-a [numbers]
  (fn prodz [h]
    (+ (pretty (. (walk h 1000) :v))
       (pretty (. (walk h 2000) :v))
       (pretty (. (walk h 3000) :v))))
  (-> numbers
      tolinked
      mix
      (linkfind 0)
      prodz))

(fn solve-b [numbers] 0)

(fn check []
  (var q (tolinked [1 2 3]))
  (assert (linkfind q 1))
  (assert (linkfind q 3))
  (assert (not (linkfind q 4)))

  (let [z (linkfind q 1)
        t (linkfind q 2)]
    (unlink z)
    (link t z)
    (linkcheck z)
    (linkcheck t)
    (assert (linkfind z 1))
    (assert (linkfind z 2))
    (assert (linkfind z 3))
    (assert (linkfind t 1))
    (assert (linkfind t 2))
    (assert (linkfind t 3)))

  (let [e (tolinked [1 2 -4])]
    (move e -4)
    (assert (= e.n.v 2))
    (assert (= e.p.v 1)))
  (let [e (tolinked [1 3 2])]
    (move e 3)
    (assert (= e.n.v 2))
    (assert (= e.p.v 1))))

{
  : check
  : read
  : solve-a
  : solve-b
}
