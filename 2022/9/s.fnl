; 2022/9

(local points (require "../lib/points"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn readline [line]
  (let [parts (str.split line)]
    { :dir (. parts 1) :count (tonumber (. parts 2))}))

(fn read [lines]
  (tbl.map lines readline))

(fn pt+ [p d]
  { :x (+ p.x d.x) :y (+ p.y d.y) })

(local dirs {
  :U {  :x  0 :y -1 }
  :L {  :x -1 :y  0 }
  :R {  :x  1 :y  0 }
  :D {  :x  0 :y  1 }

  :UL { :x -1 :y -1 }
  :UR { :x  1 :y -1 }
  :DL { :x -1 :y  1 }
  :DR { :x  1 :y  1 }

  :X {  :x  0 :y  0 }
})

(fn abs [x] (if (< x 0) (* -1 x) x))

(fn keep-up [h t]
  (let [dx (- h.x t.x)
        dy (- h.y t.y)]
    (pt+ t
      (if
        ; cardinal direction:
        (and (= dx 0) (< dy -1))      dirs.U
        (and (= dx 0) (> dy 1))       dirs.D
        (and (> dx 1) (= dy 0))       dirs.R
        (and (< dx -1) (= dy 0))      dirs.L

        ; otherwise, if distance = 2 and we didn't hit an earlier case,
        ; we're diagonal but touching - don't move
        (= (+ (abs dx) (abs dy)) 2)   dirs.X

        ; otherwise, pick the right quadrant and move diagonally
        (and (< dx 0) (< dy 0))       dirs.UL
        (and (< dx 0) (> dy 0))       dirs.DL
        (and (> dx 0) (< dy 0))       dirs.UR
        (and (> dx 0) (> dy 0))       dirs.DR

        ; ... and if we're on the same point, just don't move.
                                      dirs.X))))

(fn step [rope d]
  (tset rope 1 (pt+ (. rope 1) (. dirs d)))
  (for [i 2 (length rope) 1]
    (tset rope i (keep-up (. rope (- i 1)) (. rope i)))))

(fn marktail [pts rope]
  (let [t (. rope (length rope))]
    (points.add pts [t.x t.y 0] true)))

(fn run-instr [inst pts rope]
  (marktail pts rope)
  (for [i 1 inst.count 1]
    (step rope inst.dir)
    (marktail pts rope)))

(fn run-all [instrs len]
  (var pts (points.make))
  (var rope (fcollect [i 1 len 1] { :x 0 :y 0 }))
  (each [_ v (ipairs instrs)]
    (run-instr v pts rope))
  pts)

(fn count-points [s]
  (var r 0)
  (points.all s
    (fn [p v] (set r (+ r 1))))
  r)

(fn solve-a [x] (count-points (run-all x 2)))
(fn solve-b [x] (count-points (run-all x 10)))

(fn check []
  (fn pt [x y]
    { : x : y })
  (fn pt= [p q]
    (and (= p.x q.x) (= p.y q.y)))
  (assert (pt= (keep-up (pt 2 0) (pt 0 0)) (pt 1 0)))
  (assert (pt= (keep-up (pt 0 2) (pt 0 0)) (pt 0 1)))
  (assert (pt= (keep-up (pt 2 1) (pt 1 3)) (pt 2 2)))
  (assert (pt= (keep-up (pt 0 0) (pt 0 0)) (pt 0 0)))
  (assert (pt= (keep-up (pt 1 0) (pt 0 0)) (pt 0 0)))
  (assert (pt= (keep-up (pt 3 2) (pt 1 3)) (pt 2 2))))

{
  : check
  : read
  : solve-a
  : solve-b
}
