; 2022/8: Treetop Tree House

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn readline [line]
  (-> line
      str.explode
      (tbl.map str.tonumz)))

(fn read [lines]
  (tbl.map lines readline))

(fn in-bounds? [m p]
  (and (> p.x 0) (> p.y 0)
       (<= p.y (length m))
       (<= p.x (length (. m 1)))))

(fn point+ [p0 p1]
  { :x (+ p0.x p1.x) :y (+ p0.y p1.y) })

(fn toedge [m p d]
  "Returns the heights of the trees from p along direction d towards the
   edge, stopping at the first tree at least as tall as p."
  (var h (. m p.y p.x))
  (var r [])
  (var p (point+ p d))
  (var blocked false)
  (while (and (in-bounds? m p) (not blocked))
         (let [t (. m p.y p.x)]
           (table.insert r (. m p.y p.x))
           (when (>= t h)
                 (set blocked true)))
         (set p (point+ p d)))
  r)

(fn blocked? [m p d]
  (let [h (. m p.y p.x)]
    (tbl.find (toedge m p d) #(<= h $1))))

(local UP { :x 0 :y -1 })
(local DOWN { :x 0 :y 1 })
(local LEFT { :x -1 :y 0 })
(local RIGHT { :x 1 :y 0 })

(fn blocked-all? [m p]
  (and (blocked? m p UP)
       (blocked? m p DOWN)
       (blocked? m p LEFT)
       (blocked? m p RIGHT)))

(fn flatmap [m f]
  (var r [])
  (for [y 1 (length m) 1]
    (for [x 1 (length (. m y))]
      (table.insert r (f { : x : y }))))
  r)

(fn solve-a [m]
  (-> m
      (flatmap #(if (blocked-all? m $1) 0 1))
      tbl.sum))

(fn vdist [m p d]
  (length (toedge m p d)))

(fn viewscore [m p]
  (* (vdist m p UP)
     (vdist m p DOWN)
     (vdist m p LEFT)
     (vdist m p RIGHT)))

(fn solve-b [m]
  (-> m
      (flatmap #(viewscore m $1))
      (tbl.sorted #(> $1 $2))
      (. 1)))

(fn check []
  (let [m [[3 0 3 7 3]
           [2 5 5 1 2]
           [6 5 3 3 2]
           [3 3 5 4 9]
           [3 5 3 9 0]]
        p0 { :x 2 :y 2 }
        p1 { :x 3 :y 4 }]
    (assert (blocked? m p0 RIGHT))
    (assert (blocked? m p0 DOWN))
    (assert (not (blocked? m p0 UP)))
    (assert (not (blocked? m p0 LEFT)))

    (assert (= 2 (vdist m p1 UP)))
    (assert (= 2 (vdist m p1 LEFT)))
    (assert (= 1 (vdist m p1 DOWN)))
    (assert (= 2 (vdist m p1 RIGHT)))))

{
  : check
  : read
  : solve-a
  : solve-b
}
