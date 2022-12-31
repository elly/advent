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
    (var blocked false)
    (var p (point.add from hat))
    (while (and (not (point.eq? p to)) (not blocked))
      (when (cloud.get map p)
        (set blocked true))
      (set p (point.add p hat)))
    (not blocked)))

(fn rocks-in-sight [map p]
  (- (# (cloud.all map #(los? map p $1))) 1))

(fn solve-a [map]
  (let [(_ v) (list.maximize (cloud.all map) #(rocks-in-sight map $1))]
    v))

{
  : read
  : solve-a
}
