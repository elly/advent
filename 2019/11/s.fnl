; 2019/11: Space Police

(local cloud (require :../lib/cloud))
(local ic (require :../lib/intcode))
(local point (require :../lib/point))

(local *black* 0)
(local *white* 1)

(fn turn [dir v]
  (match [dir v]
    [:n 0] :w
    [:w 0] :s
    [:s 0] :e
    [:e 0] :n

    [:n 1] :e
    [:e 1] :s
    [:s 1] :w
    [:w 1] :n

    _ (assert false)))

(fn make-robot [start]
  (local hull (cloud.make))
  (var dir :n)
  (var loc (point.make 0 0 0))
  (var state :paint)

  (cloud.add! hull loc start)

  (fn in-hook []
    (or (cloud.get hull loc) *black*))

  (fn out-hook [v]
    (if (= state :paint)
        (do
          (cloud.add! hull loc v)
          (set state :turn))
        (do
          (set dir (turn dir v))
          (set loc (point.move loc dir 1))
          (set state :paint))))

  (values hull in-hook out-hook))

(fn run-robot [image start]
  (let [v (ic.copy image)
        (hull inh outh) (make-robot start)]
    (ic.hookio v inh outh)
    (ic.run v)
    hull))

(fn solve-a [image]
  (let [h (run-robot image *black*)]
    h.card))

(fn raster [c]
  (for [y c.bounds.y.min c.bounds.y.max 1]
    (for [x c.bounds.x.min c.bounds.x.max 1]
      (io.write (if (= (cloud.get c [x y 0]) 1) "#" ".")))
    (io.write "\n")))

(fn solve-b [image]
  (let [h (run-robot image *white*)]
    (raster h)
    h.card))

{
  :read ic.make
  : solve-a
  : solve-b
}
