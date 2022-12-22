; 2022/22: Monkey Map

(fn pk [[x y]] (.. x "/" y))
(fn pt+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(fn read [lines]
  (fn read-path [pathspec]
    (var r [])
    (each [s (pathspec:gmatch "%d+[RL]")]
      (table.insert r (tonumber (s:sub 1 (- (# s) 1))))
      (table.insert r (s:sub (# s))))
    r)

  (fn read-map [lines]
    (var m {})
    (for [y 1 (# lines) 1]
      (var x 1)
      (let [line (. lines y)]
        (for [x 1 (# line) 1]
          (tset m (pk [x y]) (line:sub x x)))))
    m)

  (local pathspec (table.remove lines (# lines)))
  (table.remove lines (# lines))

  (local path (read-path pathspec))
  (local map (read-map lines))
  { : map : path })

(fn follow-one [map you p]
  (fn turn [dir lr]
    (match dir
      [1  0]       (if (= lr :L) [0 -1] [0 1])
      [0 -1]       (if (= lr :L) [-1 0] [1 0])
      [-1 0]       (if (= lr :L) [0 1] [0 -1])
      [0  1]       (if (= lr :L) [1 0] [-1 0])))

  (fn oppose [dir]
    (match dir
      [1 0]   [-1 0]
      [0 -1]  [0 1]
      [-1 0]  [1 0]
      [0 1]   [0 -1]))

  (fn tile [p] (or (. map (pk p)) " "))

  (fn wrap []
    (let [d (oppose you.dir)]
      (var np you.loc)
      (while (not (= (tile (pt+ np d)) " "))
        (set np (pt+ np d)))
      (if (= (tile np) ".")
          (do (tset you :loc np) true)
          false)))

  (fn step []
    (let [np (pt+ you.loc you.dir)]
      (match (tile np)
        "#"        false
        "."        (do (tset you :loc np) true)
        " "        (wrap))))

  (fn steps [n]
    (var n n)
    (while (> n 0)
      (set n (- n 1))
      (when (not (step))
            (set n 0))))

  (if (= (type p) :number)
      (steps (tonumber p))
      (tset you :dir (turn you.dir p)))
  you)

(fn find-start [m]
  (var s nil)
  (for [x 1 100 1 &until s]
    (when (= (. m (pk [x 1])) ".")
          (set s [x 1])))
  s)

(fn follow [map path]
  (accumulate [you { :loc (find-start map) :dir [1 0] }
               _ p (ipairs path)]
    (follow-one map you p)))

(fn facing-index [you]
  (match you.dir
    [1 0]   0
    [0 1]   1
    [-1 0]  2
    [0 -1]  3))

(fn solve-a [spec]
  (let [you (follow spec.map spec.path)]
    (+ (* 1000 (. you.loc 2))
       (* 4 (. you.loc 1))
       (facing-index you))))

(fn solve-b [spec] 0)

(fn check [])

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
