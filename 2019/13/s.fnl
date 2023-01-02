; 2019/13: Care Package

(local cloud (require :../lib/cloud))
(local ic (require :../lib/intcode))

(fn xyv-out [f]
  (var x nil)
  (var y nil)
  (var state :x)

  (fn [v]
    (if (= state :x) (do (set x v) (set state :y))
        (= state :y) (do (set y v) (set state :v))
        (= state :v) (do (f x y v) (set state :x)))))

(fn make-arcade [image]
  (local vm (ic.copy image))
  (local screen (cloud.make))

  (fn update-screen [x y v]
    (cloud.add! screen [x y 0] v))

  (ic.hookio vm nil (xyv-out update-screen))

  {
    : vm
    : screen
  })

(fn run-arcade [a]
  (ic.run a.vm))

(fn solve-a [image]
  (let [a (make-arcade image)]
    (run-arcade a)
    (# (cloud.all a.screen #(= (cloud.get a.screen $1) 2)))))

{
  :read ic.make
  : solve-a
}
