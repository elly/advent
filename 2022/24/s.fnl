; 2022/24: Blizzard Basin

(local const (require :../lib/const))
(local str (require :../lib/str))

(fn pk [[x y]] (.. x "/" y))
(fn pk3 [[x y] s] (.. x "/" y "@" s))
(fn unpk [p] (str.allnums p))
(fn unpk3 [p]
  (let [[x y s] (str.allnums p)]
    (values [x y] s)))
(fn pt+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])
(fn pt=? [[x0 y0] [x1 y1]]
  (and (= x0 x1) (= y0 y1)))

(fn char->dir [c]
  (match c
    :< :w
    :> :e
    :^ :n
    :v :s
    _  nil))

(fn read [lines]
  (var blizzards {})
  (each [y line (ipairs lines)]
    (each [x char (ipairs (str.explode line))]
      (when (char->dir char)
            (tset blizzards (pk [x y]) [(char->dir char)]))))
  {
    : blizzards
    :rows (# lines)
    :cols (# (. lines 1))
  })

(fn step [m]
  (fn wrap [[x y]]
    [(if (= x 1) (- m.cols 1)
         (= x m.cols) 2
         x)
     (if (= y 1) (- m.rows 1)
         (= y m.rows) 2
         y)])

  (fn num-blizzards [s]
    (accumulate [t 0 _ c (pairs s)] (+ t (# c))))

  (var nb {})
  (each [b bs (pairs m.blizzards)]
    (each [_ d (ipairs bs)]
      (let [np (wrap (pt+ (unpk b) (. const.dirdelta d)))
            npk (pk np)]
        (when (not (. nb npk))
          (tset nb npk []))
        (table.insert (. nb npk) d))))
  (assert (= (num-blizzards m.blizzards) (num-blizzards nb)))
  { :blizzards nb :rows m.rows :cols m.cols })

(fn mkmodel [spec]
  (var r [])
  (var spec spec)
  (for [i 1 256 1]
    (set spec (step spec))
    (table.insert r spec.blizzards))
  { :states r :rows spec.rows :cols spec.cols })

(fn pathfind [model start end]
  (assert model)
  (assert start)
  (assert end)

  (fn in-bounds? [[x y]]
    (or (pt=? [x y] start) (pt=? [x y] end)
        (and (> x 1) (< x model.cols)
             (> y 1) (< y model.cols))))

  (fn legal? [p s]
    (assert (< s (# model.states)))
    (or (= s 0)
        (and (in-bounds? p)
             (not (. model.states s (pk p))))))

  (fn move [p d]
    (if (= d :x)
        p
        (pt+ p (. const.dirdelta d))))

  (var queue [(pk3 start 0)])
  (var pre {})
  (var visited {})
  (var done nil)
  (var ld 0)
  (var nv 0)

  (while (and (> (# queue) 0) (not done))
    (local e (table.remove queue 1))
    (when (not (. visited e))
      (local (p s) (unpk3 e))
      (when (> s ld)
        (print (.. "scan " s " " (# queue) " " nv))
        (set ld s))
      (assert (legal? p s))
      (tset visited e true)
      (set nv (+ nv 1))
      (when (pt=? p end)
            (set done s))
      (each [_ d (ipairs [:n :s :w :e :x])]
        (let [np (move p d) ns (+ s 1) nk (pk3 np ns)]
          (when (and (not (. visited nk)) (legal? np ns))
                (table.insert queue nk)
                (tset pre nk e))))))

  (var path [])
  (var endstate (pk3 end done))
  (while (not (= endstate (pk3 start 0)))
    (table.insert path endstate)
    (set endstate (. pre endstate)))
  (table.insert path endstate)
  path)


(fn solve-a [spec]
  (let [model (mkmodel spec)
        start [2 1]
        end [(- spec.cols 1) spec.rows]
        path (pathfind model start end)]
    (- (# path) 1)))

(fn solve-b [spec] 0)

(fn check [])

{
  ; :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
