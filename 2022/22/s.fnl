; 2022/22: Monkey Map

(fn pk [[x y]] (.. x "/" y))
(fn pt+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(fn read [lines]
  (fn read-path [pathspec]
    (var r [])
    (var ps pathspec)
    (while (> (# ps) 0)
      (let [(is ie) (ps:find "%d+")]
        (when (= is 1)
              (table.insert r (tonumber (ps:sub is ie)))
              (set ps (ps:sub (+ ie 1)))))
      (let [(ds de) (ps:find "[LR]")]
        (when (= ds 1)
              (table.insert r (ps:sub ds de))
              (set ps (ps:sub (+ de 1))))))
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

(fn tile [m p] (or (. m (pk p)) " "))

(fn turn [dir lr]
  (match dir
    :up (if (= lr :L) :left :right)
    :down (if (= lr :L) :right :left)
    :left (if (= lr :L) :down :up)
    :right (if (= lr :L) :up :down)))

(local *dirdelta*
  { :up [0 -1] :down [0 1] :left [-1 0] :right [1 0] })

(fn follow-one [map you p]
  (fn step []
    (var np (pt+ you.loc (. *dirdelta* you.dir)))
    (var nd you.dir)
    (when (= (tile map np) " ")
      (let [(wp wd) (map.wrap map you)]
        (set np wp)
        (set nd wd)))
    (match (tile map np)
      "#"        false
      "."        (do (tset you :loc np) (tset you :dir nd) true)
      " "        (do (pretty you) (assert false))))

  (fn steps [n]
    (var n n)
    (while (> n 0)
      (set n (- n 1))
      (when (not (step))
            (set n 0))))

  (if (= (type p) :number)
      (steps (tonumber p))
      (tset you :dir (turn you.dir p)))
  (assert (= (tile map you.loc) "."))
  ;(pretty [p you])
  you)

(fn find-start [m]
  (var s nil)
  (for [x 1 100 1 &until s]
    (when (= (. m (pk [x 1])) ".")
          (set s [x 1])))
  s)

(fn follow [map path]
  (accumulate [you { :loc (find-start map) :dir :right }
               _ p (ipairs path)]
    (follow-one map you p)))

(fn oppose [dir]
  (match dir
    :up    :down
    :down  :up
    :left  :right
    :right :left))

(fn wrap-a [map you]
  (let [d (. *dirdelta* (oppose you.dir))]
    (var np you.loc)
    (while (not (= (tile map (pt+ np d)) " "))
      (set np (pt+ np d)))
    (values np you.dir)))

(local *facing-index*
  { :right 0 :down 1 :left 2 :up 3 })

(fn solve [spec wrapf]
  (tset spec.map :wrap wrapf)
  (let [you (follow spec.map spec.path)]
    (+ (* 1000 (. you.loc 2))
       (* 4 (. you.loc 1))
       (. *facing-index* you.dir))))

(fn solve-a [spec] (solve spec wrap-a))

(fn cube-map [map]
  (fn is-test-input? [m]
    (not (. m "51/1")))

  (local test-cube-map
    {
      :fs 4
      ;   1
      ; 234
      ;   56
      :faces [[9 1] [1 5] [5 5] [9 5] [9 9] [13 9]]
      :fm [
        { :up [2 :down]  :down [4 :down]  :left [3 :down] :right [6 :left] }
        { :up [1 :down]  :down [5 :up]    :left [6 :up]   :right [3 :right] }
        { :up [1 :right] :down [5 :right] :left [2 :left] :right [4 :right] }
        { :up [1 :up]    :down [5 :down]  :left [3 :left] :right [6 :down] }
        { :up [4 :up]    :down [2 :up]    :left [3 :up]   :right [6 :right] }
        { :up [4 :right] :down [2 :left]  :left [5 :left] :right [1 :left] }
      ]
    })

  (local prod-cube-map
    {
      :fs 50
      ;  12
      ;  3
      ; 45
      ; 6
      :faces [[51 1] [101 1] [51 51] [1 101] [51 101] [1 151]]
      :fm [
        { :up [6 :right] :down [3 :down] :left [4 :right] :right [2 :right] }
        { :up [6 :up] :down [3 :left] :left [1 :left] :right [5 :left] }
        { :up [1 :up] :down [5 :down] :left [4 :down] :right [2 :up] }
        { :up [3 :right] :down [6 :down] :left [1 :right] :right [5 :right] }
        { :up [3 :up] :down [6 :left] :left [4 :left] :right [2 :right] }
        { :up [4 :up] :down [2 :down] :left [1 :down] :right [5 :up] }
      ]
    })

  (if (is-test-input? map)
      test-cube-map
      prod-cube-map))

(fn wrap-b [map you cm]
  (fn face [p]
    (var r nil)
    (each [i f (ipairs cm.faces) &until r]
      (when (and (>= (. p 1) (. f 1))
                 (>= (. p 2) (. f 2))
                 (< (. p 1) (+ (. f 1) cm.fs))
                 (< (. p 2) (+ (. f 2) cm.fs)))
        (set r i)))
    (assert r)
    r)

  (fn landing-point [f nf]
    (let [[yx yy] you.loc
          od you.dir
          nd (. nf 2)
          ocx (. cm.faces f 1)
          ocy (. cm.faces f 2)
          ncx (. cm.faces (. nf 1) 1)
          ncy (. cm.faces (. nf 1) 2)
          fx (- yx ocx)
          fy (- yy ocy)
          fs (- cm.fs 1)]
      (match [od nd]
          ; top edge -> bottom edge: preserve x, y = bottom
          [:up :up]      [(+ fx ncx) (+ fs ncy)]
          ; top edge -> top edge: high x -> low x
          [:up :down]    [(- (+ fs ncx) fx) ncy]
          ; top edge -> right edge (facing left): low x -> high y
          [:up :left]    [(+ fs ncx) (- (+ fs ncy) fx)]
          ; top edge -> left edge (facing right): low x -> low y
          [:up :right]   [ncx (+ ncy fx)]

          ; bottom edge -> bottom edge, high x -> low x
          [:down :up]                  [(- (+ fs ncx) fx) (+ fs ncy)]
          ; bottom edge -> top edge:
          [:down :down]                [(+ fx ncx) ncy]
          ; bottom edge -> right edge: low x -> low y
          [:down :left]                [(+ fs ncx) (+ ncy fx)]
          ; bottom edge -> left edge (facing right): low x -> high y
          [:down :right]               [ncx (- (+ fs ncy) fx)]

          ; left edge -> bottom edge:  low y -> high x
          [:left :up]                  [(- (+ ncx fs) fy) (+ ncy fs)]
          ; left edge -> top edge:     low y -> low x
          [:left :down]                [(+ ncx fy) ncy]
          ; left edge -> right edge:
          [:left :left]                [(+ ncx fs) (+ ncy fy)]
          ; left edge -> left edge:
          [:left :right]               [ncx (+ ncy fy)]

          ; right edge -> bottom edge:   low y -> low x
          [:right :up]                 [(+ ncx fy) (+ ncy fs)]
          ; right edge -> top edge:      low y -> high x
          [:right :down]               [(- (+ ncx fs) fy)  ncy]
          ; right edge -> right edge:
          [:right :left]               [(+ ncx fs) (+ ncy fy)]
          ; right edge -> left edge:
          [:right :right]              [ncx (+ ncy fy)]
      )))

  (let [f (face you.loc)
        nf (. cm.fm f you.dir)
        lp (landing-point f nf)]
    (pretty [you.loc f nf lp])
    (values lp (. nf 2))))

; 6276 wrong
(fn solve-b [spec]
  (let [cm (cube-map spec.map)]
    (solve spec #(wrap-b $1 $2 cm))))

(fn check [])

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
