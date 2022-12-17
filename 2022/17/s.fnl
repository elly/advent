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
  (fn []
    (let [r (. list i)]
      (set i (+ (% i (# list)) 1))
      r)))

(fn raster [map]
  (for [y 20 0 -1]
    (print
      (table.concat
        (fcollect [x 0 (- map.width 1) 1]
          (if (points.get map [x y 0]) "#" "."))))))

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

  (fn add-rock-to-map []
    (for [i 1 (# rock.points) 2]
      (points.add map
        [(+ rx (. rock.points i))
         (+ ry (. rock.points (+ i 1)))
         0]
        true))
    (when (< map.top (+ ry rock.size.y))
          (set map.top (+ ry rock.size.y))))

  (push (jets))
  (while (fall)
    (push (jets)))
  (add-rock-to-map))

(fn drop-n [map jetlist rocklist n]
  (var jet-stream (cyclic-stream jetlist))
  (var rock-stream (cyclic-stream rocklist))
  (for [i 1 n 1]
    (drop map jet-stream (rock-stream)))
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
  (let [m (make-empty-map 7)]
    (drop-n m ["v"] [r1x1] 100)
    (assert-eq m.top 100))
  (let [m (make-empty-map 7)]
    (drop-n m ["v"] [r1x2] 100)
    (assert-eq m.top 200))
  (let [m (make-empty-map 7)]
    (drop-n m ["<"] [r1x1] 100)
    (assert-eq m.top 100)
    (for [y 0 99 1]
      (assert (points.get m [0 y 0]))))
  (let [m (make-empty-map 7)]
    (drop-n m [">"] [r2x1] 100)
    (assert-eq m.top 100)
    (for [y 0 99 1]
      (assert (points.get m [6 y 0])))))

(fn solve-a [jets]
  (let [m (make-empty-map 7)]
    (drop-n m jets *rocks* 2022)
    m.top))
(fn solve-b [x] 0)

{
  : check
  : read
  : solve-a
  : solve-b
}
