; 2022/10: Cathode-Ray Tube

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn readline [line]
  (let [parts (str.split line)]
    (match parts
      ["addx" v] [:addx (str.tonumz v)]
      ["noop"]   [:noop]
      _          (assert false))))

(fn read [lines]
  (tbl.map lines readline))

(fn depipe [code]
  (var nc [])
  (each [_ insn (ipairs code)]
    (when (= (. insn 1) :addx)
          (table.insert nc [:noop]))
    (table.insert nc insn))
  nc)

(fn step [m]
  (tset m.xvals m.cycle m.x)
  (match (. m.code m.cycle)
    [:addx v] (do (tset m :x (+ m.x v)) nil)
    [:noop]   nil
    _         nil)
  (tset m :cycle (+ m.cycle 1)))

(fn done? [m]
  (> m.cycle (length m.code)))

(fn run [m]
  (while (not (done? m))
    (step m))
  (tset m.xvals m.cycle m.x)
  m.xvals)

(fn make-machine [code]
  {
    :code (depipe code)
    :cycle 1
    :x 1
    :xvals [1]
  })

(fn signal-strengths [xvals]
  (accumulate [sum 0
               _ n (ipairs [20 60 100 140 180 220])]
     (+ sum (* n (. xvals n)))))

(fn solve-a [code]
  (-> code
      make-machine
      run
      signal-strengths))

(fn raster-px [xvs x cycle]
  (fn abs [x] (if (< x 0) (* -1 x) x))
  (let [v (. xvs cycle)]
    (if (<= (abs (- x v)) 1) "#" ".")))

(fn raster [xvs]
  (local WIDTH 40)
  (for [y 1 6 1]
    (var line [])
    (for [x 1 WIDTH 1]
      (let [cycle (+ (* (- y 1) WIDTH) x)]
        (table.insert line (raster-px xvs (- x 1) cycle))))
    (print (table.concat line))))

(fn solve-b [code]
  (-> code
      make-machine
      run
      raster)
  :FZBPBFZF)

(fn check []
  (let [code [[:noop] [:addx 3] [:addx -5]]
        m (make-machine code)]
    (run m)
    (assert (tbl.aeq [1 1 1 4 4 -1] m.xvals)))
  (assert-eq (raster-px [ 1 ] 0 1) "#")
  (assert-eq (raster-px [ 1 ] 1 1) "#")
  (assert-eq (raster-px [ 1 ] 2 1) "#"))

{
  : check
  : read
  : solve-a
  : solve-b
}
