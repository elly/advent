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

(fn fetch [m]
  (when (= 0 (length m.pipe))
    (let [insn (. m.code m.pc)]
      (when (= (. insn 1) :addx)
            (table.insert m.pipe [:noop]))
      (table.insert m.pipe insn))
      (tset m :pc (+ m.pc 1))))

(fn execute [m]
  (match (. m.pipe 1)
    [:addx v] (do (tset m :x (+ m.x v)) nil)
    [:noop]   nil
    _         nil)
  (table.remove m.pipe 1)
  (tset m :cycle (+ m.cycle 1)))

(fn step [m]
  (tset m.xvals m.cycle m.x)
  (fetch m)
  (execute m))

(fn done? [m]
  (and (> m.pc (length m.code))
       (= 0 (length m.pipe))))

(fn run [m]
  (while (not (done? m))
    (step m))
  (tset m.xvals m.cycle m.x))

(fn make [code]
  {
    : code
    :cycle 1
    :pc 1
    :x 1
    :pipe []
    :xvals [1]
  })

(fn solve-a [code]
  (let [m (make code)]
    (run m)
    (accumulate [sum 0
                 _ n (ipairs [20 60 100 140 180 220])]
       (+ sum (* n (. m.xvals n))))))

(fn raster-px [xvs x cycle]
  (let [v (. xvs cycle)]
    ; if x is 1, then 0, 1, and 2 match
    (if
      (or (= x v) (= x (+ v 1)) (= x (- v 1)))
      "#"
      ".")))

(fn raster [xvs]
  (local WIDTH 40)
  (for [y 1 6 1]
    (var line [])
    (for [x 1 WIDTH 1]
      (let [cycle (+ (* (- y 1) WIDTH) x)]
        (table.insert line (raster-px xvs (- x 1) cycle))))
    (print (table.concat line))))

(fn solve-b [code]
  (let [m (make code)]
    (run m)
    (raster m.xvals)
    :FZBPBFZF))

(fn check []
  (let [code [[:noop] [:addx 3] [:addx -5]]
        m (make code)]
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
