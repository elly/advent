; 2019/10: Monitoring Station

(local cloud (require :../lib/cloud))
(local list (require :../lib/list))
(local point (require :../lib/point))

(fn read [lines]
  (var c (cloud.make))
  (for [y 1 (# lines) 1]
    (for [x 1 (# (. lines y)) 1]
      (let [a (string.sub (. lines y) x x)]
        (when (= a "#")
              (cloud.add! c [x y 0] true)))))
  c)

(fn los? [map from to]
  (let [hat (point.hat from to)]
    (var blocked (point.eq? from to))   ; no self-los
    (var p (point.add from hat))
    (while (and (not (point.eq? p to)) (not blocked))
      (when (cloud.get map p)
        (set blocked true))
      (set p (point.add p hat)))
    (not blocked)))

(fn rocks-in-sight [map p]
  (# (cloud.all map #(los? map p $1))))

(fn best-spot [map]
  (list.maximize (cloud.all map) #(rocks-in-sight map $1)))

(fn solve-a [map]
  (let [(_ v) (best-spot map)]
    v))

(fn zap [map laser]
  "Completes one laser cycle, returning a new map and a list of vaporized
   rocks."
  (local [lx ly lz] laser)

  (fn abs [v] (if (< v 0) (* -1 v) v))

  (fn upright? [px py] (and (>= px lx) (<= py ly)))
  (fn downright? [px py] (and (>= px lx) (>= py ly)))
  (fn downleft? [px py] (and (<= px lx) (>= py ly)))
  (fn upleft? [px py] (and (<= px lx) (<= py ly)))

  (fn slope [[px py pz]]
    (let [dx (- px lx) dy (- py ly)
          adx (abs dx) ady (abs dy)]
      (match [dx dy]
        (where [0 y] (< y 0))             0
        (where [x 0] (> x 0))             1000
        (where [0 y] (> y 0))             2000
        (where [x 0] (< x 0))             3000
        (where [x y] (upright? px py))    (/ adx ady)
        (where [x y] (downright? px py))  (+ 1000 (/ ady adx))
        (where [x y] (downleft? px py))   (+ 2000 (/ adx ady))
        (where [x y] (upleft? px py))     (+ 3000 (/ ady adx)))))
  
  (fn slope< [p q]
    (< (slope p) (slope q)))

  (var targets (cloud.all map #(los? map laser $1)))
  (table.sort targets slope<)

  (local nmap (cloud.of (cloud.all map #(not (los? map laser $1)))))
  (assert (= (+ (# targets) nmap.card) map.card))

  (values nmap targets))

(fn zap-all [map laser]
  (var map map)
  (var zapped [])
  (while (> map.card 1)
    (let [(nm nz) (zap map laser)]
      (set map nm)
      (each [_ z (ipairs nz)] (table.insert zapped z))))
  zapped)

(fn solve-b [map]
  (let [(p _) (best-spot map)
        r (zap-all map p)
        v (. r 200)]
    (+ (* 100 (- (. v 1) 1)) (- (. v 2) 1))))

{
  : read
  : solve-a
  : solve-b
}
