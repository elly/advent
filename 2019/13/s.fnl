; 2019/13: Care Package

(local cloud (require :../lib/cloud))
(local ic (require :../lib/intcode))
(local point (require :../lib/point))

(local *empty* 0)
(local *wall* 1)
(local *block* 2)
(local *paddle* 3)
(local *ball* 4)

(local *joy-left* -1)
(local *joy-right* 1)
(local *joy-none* 0)

(fn xyv-out [f]
  (var x nil)
  (var y nil)
  (var state :x)

  (fn [v]
    (if (= state :x) (do (set x v) (set state :y))
        (= state :y) (do (set y v) (set state :v))
        (= state :v) (do (f x y v) (set state :x)))))

(fn make-arcade [image infunc]
  (local vm (ic.copy image))
  (local screen (cloud.make))
  (local arcade {
    : vm
    : screen
    :nblock 0
    :score 0
    :ball (point.make -1 -1 0)
    :ball-prev (point.make -1 -1 0)
    :paddle (point.make -1 -1 0)
    :y-max 0
  })

  (fn update-screen [x y v]
    (when (= v *paddle*)
      (tset arcade :paddle [x y 0])
      (tset arcade :y-max (- y 1)))
    (when (= v *ball*)
      (tset arcade :ball-prev arcade.ball)
      (tset arcade :ball [x y 0]))
    (when (and (not (= v *block*)) (= (cloud.get screen [x y 0]) *block*))
      (tset arcade :nblock (- arcade.nblock 1)))
    (when (= v *block*)
      (tset arcade :nblock (+ arcade.nblock 1)))
    (cloud.add! screen [x y 0] v))

  (fn draw [x y v]
    (if (= x -1)
        (tset arcade :score v)
        (update-screen x y v)))

  (ic.hookio vm #(infunc arcade) (xyv-out draw))

  arcade)

(fn all-blocks [a]
  (cloud.all a.screen #(= (cloud.get a.screen $1) *block*)))

(fn run-arcade [a]
  (ic.run a.vm))

(fn solve-a [image]
  (let [a (make-arcade image nil)]
    (run-arcade a)
    (# (all-blocks a))))

(fn cast [map bpos bvec ep]
  (fn xpart [[x y z]] [x 0 0])
  (fn ypart [[x y z]] [0 y 0])

  (fn xreflect [[dx dy dz]] [(* -1 dx) dy dz])
  (fn yreflect [[dx dy dz]] [dx (* -1 dy) dz])
  (fn xyreflect [[dx dy dz]] [(* -1 dx) (* -1 dy) dz])

  (fn bouncy? [c]
    (or (= c *wall*) (= c *block*)))

  (fn cast1 [map b v]
    (let [nbx (point.add b (xpart v))
          nby (point.add b (ypart v))
          nbxy (point.add b v)
          cx (cloud.get map nbx)
          cy (cloud.get map nby)
          cxy (cloud.get map nbxy)]
      (var nv v)
      (var nb b)
      (when (= cxy *block*)
        (cloud.add! map nbxy *empty*))
      (when (= cy *block*)
        (cloud.add! map nby *empty*))
      (when (= cx *block*)
        (cloud.add! map nbx *empty*))
      (if
        (and (bouncy? cx) (bouncy? cy))  (set nv (xyreflect nv))
        (bouncy? cy)                     (set nv (yreflect nv))
        (bouncy? cx)                     (set nv (xreflect nv))
        (bouncy? cxy)                    (set nv (xyreflect nv))
                                         (set nb (point.add b nv)))
      (values nb nv)))

  (var map (cloud.copy map))
  (var b bpos)
  (var v bvec)
  (while (not (ep b))
    (let [(nb nv) (cast1 map b v)]
      (assert (< (. nb 2) 100))
      (set b nb)
      (set v nv)))
  b)

(fn win-game [a]
  (fn p->x [[x y z]] x)
  (fn p->y [[x y z]] y)

  (fn move-towards [x]
    (let [ax (p->x a.paddle)]
      (if (> x ax) *joy-right*
          (< x ax) *joy-left*
                   *joy-none*)))

  (let [bp a.ball
        bv (point.hat a.ball-prev a.ball)]
    (if (> (p->y a.ball-prev) 0)
        (move-towards (p->x (cast a.screen bp bv #(= a.y-max (p->y $1)))))
        *joy-none*)))

(fn solve-b [image]
  (let [a (make-arcade image win-game)]
    (ic.poke a.vm 0 2)
    (run-arcade a)
    a.score))

{
  :read ic.make
  : solve-a
  : solve-b
}
