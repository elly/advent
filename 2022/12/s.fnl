; 2022/12

(local const (require "../lib/const"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn ord [c] (string.byte c 1))

(fn pointkey [[x y]]
  (.. x "," y))

(fn neighbors [m p]
  (fn point+ [[p0x p0y] [p1x p1y]]
    [(+ p0x p1x) (+ p0y p1y)])

  (fn map-ref [m p]
    (. m :map (. p 2) (. p 1)))

  (fn in-bounds? [m [x y]]
    (and (> y 0) (<= y (length m.map))
         (> x 0) (<= x (length (. m.map 1)))))

  (fn can-climb? [m p0 p1]
    (let [d (- (map-ref m p1) (map-ref m p0))]
      (>= d -1)))

  (fn can-go? [p1]
    (and (in-bounds? m p1) (can-climb? m p p1)))

  (tbl.filter
    (tbl.map const.dirmods #(point+ p $1))
    can-go?))

(fn add-bfs! [m]
  (var q [m.end])
  (var visited { (pointkey m.end) true })
  (var parents {})
  (while (> (length q) 0)
    (let [v (table.remove q 1)]
      (each [_ w (ipairs (neighbors m v))]
        (when (not (. visited (pointkey w)))
              (tset visited (pointkey w) true)
              (tset parents (pointkey w) (pointkey v))
              (table.insert q w)))))
  (tset m :parents parents)
  m)

(fn read [lines]
  (fn readline [line]
    (-> line
        str.explode))

  (fn build-map-square! [r m y x]
    (var c (. m y x))
    (when (= c :S) (tset r :begin [x y]))
    (when (= c :E) (tset r :end [x y]))
    (tset r :map y x
      (match c
        :S 0
        :E 25
        _ (- (ord c) (ord :a)))))

  (fn build-map! [m]
    (var r { :map [] })
    (for [y 1 (length m) 1]
      (tset r :map y [])
      (for [x 1 (length (. m y)) 1]
        (build-map-square! r m y x)))
    r)

  (-> lines
      (tbl.map readline)
      build-map!
      add-bfs!))

(fn bestpath [m b]
  (var p [])
  (var c (pointkey b))
  (var s (pointkey m.end))
  (while (not (= s c))
         (table.insert p c)
         (set c (. m.parents c))
         (assert c))
  p)

(fn solve-a [m]
  (length (bestpath m m.begin)))

(fn all-as [m]
  (var r [])
  (for [y 1 (length m.map) 1]
    (for [x 1 (length (. m.map 1)) 1]
      (when (= (. m.map y x) 0)
            (table.insert r [x y]))))
  r)

(fn solve-b [m]
  (fn path-score [a]
    (assert (= (type a) :table))
    (length (bestpath m a)))
  (-> m
      all-as
      (tbl.filter #(. m.parents (pointkey $1)))
      (tbl.maxval #(- 1000 (path-score $1)))
      path-score))

{
  : read
  : solve-a
  : solve-b
}
