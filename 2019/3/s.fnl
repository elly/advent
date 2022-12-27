; 2019/3: Crossed Wires

(local cloud (require :../lib/cloud))
(local point (require :../lib/point))

(fn read [lines]
  (fn read-segment [s]
    [
      (tonumber (s:sub 2))
      (match (s:sub 1 1)
        :U :n
        :D :s
        :L :w
        :R :e)
    ])

  (fn read-wire [w]
    (icollect [v (w:gmatch "[UDLR]%d+")] (read-segment v)))

  [(read-wire (. lines 1))
   (read-wire (. lines 2))])

(fn wire->points [w]
  (var c (cloud.make))
  (var p (point.make 0 0 0))
  (var s 0)

  (fn walk-seg [[n d]]
    (for [i 1 n 1]
      (set p (point.move p d))
      (set s (+ s 1))
      (cloud.add! c p s)))

  (each [_ seg (ipairs w)]
    (walk-seg seg))
  c)

(fn join-wires [w1 w2 f]
  (let [c1 (wire->points w1) c2 (wire->points w2)]
    (cloud.intersect c1 c2 f)))

(fn solve-a [[w1 w2]]
  (local *zero* (point.make 0 0 0))
  (fn distance-from-zero< [p0 p1]
    (< (point.manhattan p0 *zero*) (point.manhattan p1 *zero*)))
  (let [in (cloud.all (join-wires w1 w2))]
    (table.sort in distance-from-zero<)
    (point.manhattan (. in 1) *zero*)))

(fn solve-b [[w1 w2]]
  (let [c (join-wires w1 w2 #(+ $1 $2))
        in (cloud.all c)]
    (table.sort in #(< (cloud.get c $1) (cloud.get c $2)))
    (cloud.get c (. in 1))))

{
  : read
  : solve-a
  : solve-b
}
