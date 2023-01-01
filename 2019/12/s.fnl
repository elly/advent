; 2019/12: The N-Body Problem

(local point (require :../lib/point))

(fn read [lines]
  (icollect [_ line (ipairs lines)]
    {
      :pos (icollect [c (line:gmatch "-?%d+")] (tonumber c))
      :vel [0 0 0]
    }))

(fn step [moons]
  (local moons
    (icollect [_ m (ipairs moons)]
      {
        :pos (icollect [_ d (ipairs m.pos)] d)
        :vel (icollect [_ d (ipairs m.vel)] d)
      }))

  (fn apply-gravity-axis [m o a]
    (let [mp (. m.pos a) op (. o.pos a) mv (. m.vel a)]
      (if (> mp op) (- mv 1)
          (< mp op) (+ mv 1)
                    mv)))

  (fn apply-gravity-between! [m o]
    (tset m :vel
      [(apply-gravity-axis m o 1)
       (apply-gravity-axis m o 2)
       (apply-gravity-axis m o 3)]))

  (fn apply-gravity! [moon]
    (each [_ o (ipairs moons)]
      (apply-gravity-between! moon o)))

  (fn apply-velocity! [moon]
    (tset moon :pos (point.add moon.pos moon.vel)))

  (each [_ m (ipairs moons)]
    (apply-gravity! m))
  (each [_ m (ipairs moons)]
    (apply-velocity! m))

  moons)

(fn step-n [moons n]
  (var moons moons)
  (for [i 1 n 1]
    (set moons (step moons)))
  moons)

(fn energy [moons]
  (fn vsum [v]
    (accumulate [s 0 _ d (ipairs v)] (+ s (math.abs d))))

  (fn potential-energy [m] (vsum m.pos))
  (fn kinetic-energy [m] (vsum m.vel))

  (accumulate [s 0 _ m (ipairs moons)]
    (+ s (* (potential-energy m) (kinetic-energy m)))))

(fn solve-a [moons]
  (energy (step-n moons 1000)))

(fn solve-b [moons] 0)

{
  : read
  : solve-a
  : solve-b
}
