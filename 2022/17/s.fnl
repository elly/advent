; 2022/17
; Falling rocks!
; We're going to represent rocks as a set of offsets from their top-left corner
; and use a coordinate system where y increases going upwards. We'll also give
; each rock a bounding box to make checking for collisions easier.

(local points (require :../lib/points))
(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(local *rocks*
 [
   {
     :points [0 0 1 0 2 0 3 0]
     :size { :x 4 :y 1 }
   }
   {
     :points [1 0 0 1 1 1 2 1 1 2]
     :size { :x 3 :y 3 }
   }
   {
     :points [0 0 1 0 2 0 2 1 2 2]
     :size { :x 3 :y 3 }
   }
   {
     :points [0 0 0 1 0 2 0 3]
     :size { :x 1 :y 4 }
   }
   {
     :points [0 0 0 1 1 0 1 1]
     :size { :x 2 :y 2 }
   }
 ])

(fn cyclic-stream [list]
  (var i 1)
  (fn [z]
    (if z
        i
       (let [r (. list i)]
         (set i (+ (% i (# list)) 1))
         r))))

(fn raster [map]
  (for [y 20 0 -1]
    (print
      (table.concat
        (fcollect [x 0 (- map.width 1) 1]
          (if (points.get map [x y 0]) "#" "."))))))

(fn add-rock-to-map [map rock rx ry]
  (for [i 1 (# rock.points) 2]
    (points.add map
      [(+ rx (. rock.points i))
       (+ ry (. rock.points (+ i 1)))
       0]
      true))
  (when (< map.top (+ ry rock.size.y))
        (set map.top (+ ry rock.size.y))))

(fn drop [map jets rock]
  (var rx 2)
  (var ry (+ map.top 3))

  (fn can-move? [dx dy]
    "We're stopped if the space any of our rock spaces would move to is rock."
    (var blocked (and (<= ry 0) (not (= dy 0))))
    (for [i 1 (# rock.points) 2 &until blocked]
      (let [x (+ rx (. rock.points i))
            y (+ ry (. rock.points (+ i 1)))]
        (when (points.get map [(+ x dx) (+ y dy) 0])
              (set blocked true))))
    (not blocked))

  (fn push [j]
    (let [dx (if (= j "<") -1 (= j ">") 1 true 0)]
      (when (and (>= (+ rx dx) 0)
                 (<= (+ rx rock.size.x dx) map.width)
                 (can-move? dx 0))
            (set rx (+ rx dx)))))

  (fn fall []
    (if
      (can-move? 0 -1)
      (do
        (set ry (- ry 1))
        true)
      false))

  (push (jets))
  (while (fall)
    (push (jets)))
  (values rx ry))

(fn drop-n [map jet-stream rock-stream n]
  (for [i 1 n 1]
    (let [rock (rock-stream)
          (rx ry) (drop map jet-stream rock)]
      (add-rock-to-map map rock rx ry)))
  map)

(fn make-empty-map [width]
  {
    :cells (points.make)
    :width width
    :top 0
  })

(fn read [lines] (str.explode (. lines 1)))

(fn check []
  (var r1x1 { :points [0 0] :size { :x 1 :y 1 } })
  (var r1x2 { :points [0 0 0 1] :size { :x 1 :y 2 } })
  (var r2x1 { :points [0 0 1 0] :size { :x 2 :y 1 } })
  (fn drn [m js rs n]
    (drop-n m (cyclic-stream js) (cyclic-stream rs) n))
  (let [m (make-empty-map 7)]
    (drn m ["v"] [r1x1] 100)
    (assert-eq m.top 100))
  (let [m (make-empty-map 7)]
    (drn m ["v"] [r1x2] 100)
    (assert-eq m.top 200))
  (let [m (make-empty-map 7)]
    (drn m ["<"] [r1x1] 100)
    (assert-eq m.top 100)
    (for [y 0 99 1]
      (assert (points.get m [0 y 0]))))
  (let [m (make-empty-map 7)]
    (drn m [">"] [r2x1] 100)
    (assert-eq m.top 100)
    (for [y 0 99 1]
      (assert (points.get m [6 y 0])))))

(fn solve-a [jets]
  (let [m (make-empty-map 7)]
    (drop-n m (cyclic-stream jets) (cyclic-stream *rocks*) 2022)
    m.top))

(fn find-period [jets]
  (var jet-stream (cyclic-stream jets))
  (var rock-stream (cyclic-stream *rocks*))
  (var m (make-empty-map 7))
  (var jsi [])
  (for [i 1 10000 1]
    (drop-n m jet-stream rock-stream 1)
    (table.insert jsi (jet-stream true)))

  (var found-preamble nil)
  (var found-period nil)

  (fn has-period? [pre per]
    (var good true)
    (for [i 1 30 1]
      (when (not (= (. jsi (+ pre i)) (. jsi (+ pre per i))))
            (set good false)))
    good)

  (for [preamble 0 (# jets) 1 &until found-preamble]
    (for [period-length 1 (# jets) 1 &until found-preamble]
      (when (has-period? preamble period-length)
            (set found-preamble preamble)
            (set found-period period-length))))
  (values found-preamble found-period))

(fn solve-b [jets]
  (fn height-after [n]
    (var m (make-empty-map 7))
    (drop-n m (cyclic-stream jets) (cyclic-stream *rocks*) n)
    m.top)
  (let [nrocks 1000000000000
        (e p) (find-period jets)
        r     (% (- nrocks e) p)
        eh    (height-after (+ e p))
        ph    (- (height-after (+ e p p)) eh)
        rh    (- (height-after (+ e p p r)) (+ eh ph))
        fp    (math.floor (/ (- nrocks (+ e p)) p))]
    (+ eh (* fp ph) rh)))


; For part b: for the state to recur, we need:
; 1. The same stream of rocks
; 2. The same stream of jets
; 3. The same last part of the cavern, which is the area above the lowest
;    section of floor
; Experimentally (3) happens ~never - I ran 100 periods for (* jets rocks)
; and never saw the same distance-to-floor, let alone the same structure above
; it. So, that's not going to work.
;
; How about: how far the first rock (dropped now) would fall below the top? If
; the answer to *that* varies, we can find one where it is 0, and then treat
; that as the start of a new recurrence cycle?
;
; Those are basically always very small numbers (like, [0 1 2]) but do not show
; any obvious recurrence pattern. What if we look at the top n rows only?
;
; There are some apparent patterns, but they don't seem to hold up in the long
; run and I'm not sure why. For example, after 9 periods the top 4 rows match
; those after 5 periods, and that holds for 13 and 17 but then not for 21.
;
; If I try "top 6 rows" instead, 13 and 5 match, 23 and 18 match, 33 and 17
; match, but there's no particular reason to think any of these are big enough
; windows for an actual recurrence. Even using top *10* rows, there's a
; recurrence between 23 and 18, but there's none between 28 and 23, so that
; won't work either.
;
; Could I do direct simulation? if I try dropping #jets * #rocks * 10 rocks,
; that takes 12 seconds, so a direct sim (if I figured out how to compact the
; map) would take 12 million seconds (ish), which is way too long.

; 5 9 14 21 25 29 33 38 4 13 17 21 26 39
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29 33 38 2 11 16 20 24 29 35 6 12 17 21 25 35 39 4 10 16 22 26 31 37 3 11 15 19 24 37
; 3 7 11 15 19 29

{
  : check
  : read
  : solve-a
  : solve-b
}
