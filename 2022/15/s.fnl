; 2022/15: Beacon Exclusion Zone

(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn abs [x]
  (if (< x 0) (* -1 x)
              x))

(fn point=? [[p0x p0y] [p1x p1y]]
  (and (= p0x p1x) (= p0y p1y)))

(fn manhattan [[sx sy] [px py]]
  (+ (abs (- px sx))
     (abs (- py sy))))

(fn read [lines]
  (fn read-sensor [line]
    (let [[sx sy bx by] (str.allnums line)]
      {
        :pos [sx sy]
        :nearest [bx by]
        :clear-range (manhattan [sx sy] [bx by])
      }))
  (tbl.map lines read-sensor))

(fn x-bounds [sensors]
  (fn low-x [s] (- (. s.pos 1) s.clear-range))
  (fn high-x [s] (+ (. s.pos 1) s.clear-range))
  (fn first [x] (. x 1))
  (fn last [x] (. x (# x)))
  (let [low-xes (tbl.map sensors low-x)
        high-xes (tbl.map sensors high-x)]
    (values
      (first (tbl.sorted low-xes))
      (last (tbl.sorted high-xes)))))

(fn in-range-of-any? [sensors x y]
  (var in-range false)
  (let [p [x y]]
    (each [_ s (ipairs sensors) &until in-range]
      (when (and (<= (manhattan s.pos p) s.clear-range)
                 (not (point=? s.nearest p)))
        (set in-range true))))
  in-range)

(fn is-test-input? [sensors]
  (= (. sensors 1 :pos 1) 2))

(fn check [])
(fn solve-a [sensors]
  (let [(xl xh) (x-bounds sensors)
        y (if (is-test-input? sensors) 10 2000000)]
    (var n 0)
    (for [x xl xh 1]
      (when (in-range-of-any? sensors x y)
            (set n (+ n 1))))
    n))
(fn solve-b [x] 0)

{
  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
