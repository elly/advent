; 2022/18: Boiling Boulders

(local const (require :../lib/const))
(local points (require :../lib/points))
(local str (require :../lib/str))

(fn in-box? [b [x y z]]
  (and (>= x b.x.min) (<= x b.x.max)
       (>= y b.y.min) (<= y b.y.max)
       (>= z b.z.min) (<= z b.z.max)))

(fn outset [bb]
  { :x { :min (- bb.x.min 1) :max (+ bb.x.max 1) }
    :y { :min (- bb.y.min 1) :max (+ bb.y.max 1) }
    :z { :min (- bb.z.min 1) :max (+ bb.z.max 1) } })

(fn point3+ [[x0 y0 z0] [x1 y1 z1]]
  [(+ x0 x1) (+ y0 y1) (+ z0 z1)])

(fn read [lines]

  (fn fill-box [m b t]
    (for [x b.x.min b.x.max 1]
      (for [y b.y.min b.y.max 1]
        (for [z b.z.min b.z.max 1]
          (when (not (points.get m [x y z]))
                (points.add m [x y z] :air))))))

  (fn flood-fill [m bb from to start]
    (var q [start])
    (while (> (# q) 0)
      (let [p (table.remove q 1)]
        (when (and (in-box? bb p) (= (points.get m p) from))
              (points.add m p to)
              (each [_ d (ipairs const.dirmods3)]
                (table.insert q (point3+ p d)))))))

  (var m (points.make))
  (each [_ line (ipairs lines)]
    (points.add m (str.allnums line) :lava))
  (let [bb (outset (points.bounding-box m))]
    (fill-box m bb :air)
    (flood-fill m bb :air :steam [bb.x.min bb.y.min bb.z.min]))
  m)

(fn open-faces [m p open?]
  (accumulate [s 0 _ d (ipairs const.dirmods3)]
    (if (not (open? (points.get m (point3+ p d))))
        s
        (+ s 1))))

(fn surface-area [m f]
  (var t 0)
  (points.all m
    (fn [p]
      (when (= (points.get m p) :lava)
            (set t (+ t (open-faces m p f))))))
  t)

(fn check []
  (assert-eq 6 (open-faces {} [0 0 0] #(= $1 nil))))

(fn solve-a [model] (surface-area model #(or (not $1) (= $1 :steam) (= $1 :air))))
(fn solve-b [model] (surface-area model #(or (not $1) (= $1 :steam))))

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
