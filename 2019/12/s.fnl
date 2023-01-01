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

(fn slice [moons dim]
  (icollect [_ m (ipairs moons)]
    [(. m.pos dim) (. m.vel dim)]))

(fn recur [smoons]
  (fn freeze [st]
    (table.concat
      (icollect [_ s (ipairs st)]
        (.. (. s 1) "/" (. s 2)))
      ","))

  (fn rgrav [sp st]
    (accumulate [s 0 _ o (ipairs st)]
      (let [op (. o 1)]
        (if (> op sp) (+ s 1)
            (< op sp) (- s 1)
                      s))))

  (fn rstep [st]
    (icollect [_ s (ipairs st)]
      (let [g (rgrav (. s 1) st)]
        [(+ (. s 1) (. s 2) g)
         (+ (. s 2) g)])))

  (local visited {})
  (var st smoons)
  (var n 0)
  (var done false)
  (while (not done)
    (let [fs (freeze st)]
      (if (. visited fs)
          (set done true)
          (do (tset visited fs n)
              (set n (+ n 1))
              (set st (rstep st))))))
  (assert (= 0 (. visited (freeze st))))
  n)

(fn lcm [a b c]
  (fn gcd [a b]
    (if (= b 0)
        a
        (gcd b (% a b))))

  (fn lcm2 [a b]
    (* a (/ b (gcd a b))))

  (lcm2 (lcm2 a b) c))

(fn solve-b [moons]
  ; Key insight: all three dimensions are independent - the system has an x
  ; recurrence, a y recurrence, and a z recurrence. We can split it up into
  ; three separate simpler systems, find their recurrence periods, then find
  ; the LCM of those three.
  (let [rx (recur (slice moons 1))
        ry (recur (slice moons 2))
        rz (recur (slice moons 3))]
    (lcm rx ry rz)))

{
  : read
  : solve-a
  : solve-b
}
