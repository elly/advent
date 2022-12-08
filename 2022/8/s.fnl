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

(fn mat [m p]
  (. m p.y p.x))

(fn blocked? [m p d]
  (var h (. m p.y p.x))
  (var p (pt+ p d))
  (var b false)
  (while (in-bounds? m p)
    (if (>= (mat m p) h)
      (set b true))
    (set p (pt+ p d)))
  b)

(local UP { :x 0 :y -1 })
(local DOWN { :x 0 :y 1 })
(local LEFT { :x -1 :y 0 })
(local RIGHT { :x 1 :y 0 })

(fn blocked-all? [m p]
  (and (blocked? m p UP)
       (blocked? m p DOWN)
       (blocked? m p LEFT)
       (blocked? m p RIGHT)))

(fn nblocked [m]
  (var r 0)
  (for [y 1 (length m) 1]
    (for [x 1 (length (. m y)) 1]
      (let [p { : x : y }]
        (when (not (blocked-all? m p))
              (set r (+ r 1))))))
  r)

(fn solve-a [x] (nblocked x))

(fn vdist [m p d]
  (var r 0)
  (var p p)
  (var h (. m p.y p.x))
  (var keep-going true)
  (while (and (in-bounds? m p) keep-going)
    (set p (pt+ p d))
    (when (in-bounds? m p)
          (set r (+ r 1))
          (if (>= (. m p.y p.x) h)
              (set keep-going false))))
  r)

(fn viewscore [m p]
  (* (vdist m p UP)
     (vdist m p DOWN)
     (vdist m p LEFT)
     (vdist m p RIGHT)))

(fn bestviewscore [m]
  (var b 0)
  (for [y 2 (- (length m) 1) 1]
    (for [x 2 (- (length (. m y)) 1) 1]
      (let [p { : x : y }
            s (viewscore m p)]
        (when (> s b)
              (set b s)))))
  b)

(fn solve-b [x] (bestviewscore x))

(fn check [])

{
  : check
  : read
  : solve-a
  : solve-b
}
