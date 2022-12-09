; 2022/9

(local points (require "../lib/points"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn readline [line]
  (let [parts (str.split line)]
    { :dir (. parts 1) :count (tonumber (. parts 2))}))

(fn read [lines]
  (tbl.map lines readline))

(fn pt [x y]
  { : x : y })
(fn pt= [p q]
  (and (= p.x q.x) (= p.y q.y)))
(fn pt+ [p d]
  { :x (+ p.x d.x) :y (+ p.y d.y) })

(local dirs {
  :U { :x 0 :y -1 }
  :L { :x -1 :y 0 }
  :R { :x 1 :y 0 }
  :D { :x 0 :y 1 }

  :UL { :x -1 :y -1 }
  :UR { :x 1  :y -1 }
  :DL { :x -1 :y 1 }
  :DR { :x 1 :y 1 }
})

(fn abs [x] (if (< x 0) (* -1 x) x))

(fn keep-up [h t]
  (let [dx (- h.x t.x)
        dy (- h.y t.y)
        td (+ (abs dx) (abs dy))]
    (if
      (and (= dx 0) (< dy -1)) (pt+ t dirs.U)
      (and (= dx 0) (> dy 1)) (pt+ t dirs.D)
      (and (> dx 1) (= dy 0)) (pt+ t dirs.R)
      (and (< dx -1) (= dy 0)) (pt+ t dirs.L)
      (= td 2) t
      (and (< dx 0) (< dy 0)) (pt+ t dirs.UL)
      (and (< dx 0) (> dy 0)) (pt+ t dirs.DL)
      (and (> dx 0) (< dy 0)) (pt+ t dirs.UR)
      (and (> dx 0) (> dy 0)) (pt+ t dirs.DR)
      t)))

(fn step [rope d]
  (tset rope 1 (pt+ (. rope 1) (. dirs d)))
  (for [i 2 (length rope) 1]
    (tset rope i (keep-up (. rope (- i 1)) (. rope i)))))

(fn run-instr [inst pts rope]
  (let [t (. rope (length rope))]
    (points.add pts [t.x t.y 0] true))
  (for [i 1 inst.count 1]
    (step rope inst.dir)
    (let [nt (. rope (length rope))]
      (points.add pts [nt.x nt.y 0] true))))

(fn run-all [instrs len]
  (var pts (points.make))
  (var rope (fcollect [i 1 len 1] (pt 0 0)))
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
