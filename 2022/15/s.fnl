; 2022/15: Beacon Exclusion Zone

(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn abs [x]
  (if (< x 0) (* -1 x)
              x))

(fn manhattan [sx sy px py]
  (+ (abs (- px sx))
     (abs (- py sy))))

(fn read [lines]
  (fn read-sensor [line]
    (let [[sx sy bx by] (str.allnums line)]
      [
        sx sy
        bx by
        (manhattan sx sy bx by)
      ]))
  (tbl.map lines read-sensor))

(fn x-bounds [sensors]
  (fn low-x [s] (- (. s 1) (. s 5)))
  (fn high-x [s] (+ (. s 1) (. s 5)))
  (fn first [x] (. x 1))
  (fn last [x] (. x (# x)))
  (let [low-xes (tbl.map sensors low-x)
        high-xes (tbl.map sensors high-x)]
    (values
      (first (tbl.sorted low-xes))
      (last (tbl.sorted high-xes)))))

(fn could-be-here? [sensors x y]
  (var in-range false)
  (each [_ s (ipairs sensors) &until in-range]
    (when (<= (manhattan (. s 1) (. s 2) x y) (. s 5))
      (set in-range true)))
  (not in-range))

(fn beacon-known-at? [sensors x y]
  (var known false)
  (each [_ s (ipairs sensors) &until known]
    (when (and (= (. s 3) x) (= (. s 4) y))
          (set known true)))
  known)

(fn is-test-input? [sensors]
  (= (. sensors 1 1) 2))

(fn check []
  (assert (not (could-be-here? [[2 0 2 10 10]] 2 10))))

(fn solve-a [sensors]
  (let [(xl xh) (x-bounds sensors)
        y (if (is-test-input? sensors) 10 2000000)]
    (var n 0)
    (for [x xl xh 1]
      (when (and (not (could-be-here? sensors x y))
                 (not (beacon-known-at? sensors x y)))
            (set n (+ n 1))))
    n))

; Some part b notes;
; The areas of the sensors can't overlap because if they did they'd detect the
; same beacon - at most they can overlap at a single point. Also I can cheaply
; detect whether this happens, and it does happen a lot in the real input.
;
; Also, as a note to self, I hacked up a fast C solution using my original
; algorithm here, and it probably won't work - it takes around 37ms per row to
; scan, which makes the whole problem take around 41 hours. So, no dice there,
; and back to the thinking board.
(fn solve-b [sensors]
  (for [x 0 20 1]
    (for [y 0 20 1]
      (when (could-be-here? sensors x y)
            (pretty [x y]))))
  0)

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
