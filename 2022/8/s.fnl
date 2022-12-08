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

(fn pt+ [p0 p1]
  { :x (+ p0.x p1.x) :y (+ p0.y p1.y) })

(fn toedge [m p d]
  (var r [])
  (var p (pt+ p d))
  (while (in-bounds? m p)
         (table.insert r (. m p.y p.x))
         (set p (pt+ p d)))
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

(fn nblocked [m]
  (-> m
      (flatmap #(if (blocked-all? m $1) 0 1))
      tbl.sum))

(fn solve-a [x] (nblocked x))

(fn vdist [m p d]
  (let [h (. m p.y p.x)
        trees (toedge m p d)
        e (tbl.indexf trees #(>= $1 h))]
    (or e (length trees))))

(fn viewscore [m p]
  (* (vdist m p UP)
     (vdist m p DOWN)
     (vdist m p LEFT)
     (vdist m p RIGHT)))

(fn bestviewscore [m]
  (-> m
      (flatmap #(viewscore m $1))
      (tbl.sorted #(> $1 $2))
      (. 1)))

(fn solve-b [x] (bestviewscore x))

(fn check [])

{
  : check
  : read
  : solve-a
  : solve-b
}
