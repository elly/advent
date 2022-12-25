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
            (tset blizzards (pk [x y]) (char->dir char)))))
  {
    : blizzards
    :rows (# lines)
    :cols (# (. lines 1))
  })

(fn mkmodel [spec]
  "We can compute in advance which turns will allow moving to a square, by
   knowing when blizzards return to the square."
  (fn move-blizzard [[x y] b]
    (let [mr (- spec.rows 1) mc (- spec.cols 1)]
      (match [x y b]
        [_ mr :s] [x 2]
        [_  2 :n] [x mr]
        [mc _ :e] [2 y]
        [2  _ :w] [mc y]
        [x y d] (pt+ [x y] (. const.dirdelta b)))))

  (fn add-blizzard [rs p b]
    (local xykey (if (or (= b :n) (= b :s)) :y :x))
    (var np (move-blizzard p b))
    (var d 0)
    (tset (. rs (pk p) xykey) 0 true)
    (while (not (pt=? p np))
      (set d (+ d 1))
      (tset (. rs (pk np) xykey) d true)
      (set np (move-blizzard np b))))

  (var rs { :x (- spec.cols 2) :y (- spec.rows 2) })
  (for [y 2 (- spec.rows 1) 1]
    (for [x 2 (- spec.cols 1) 1]
      (tset rs (pk [x y]) { :x {} :y {} })))

  (for [y 2 (- spec.rows 1) 1]
    (for [x 2 (- spec.cols 1) 1]
      (let [b (. spec.blizzards (pk [x y]))]
        (when b
          (add-blizzard rs [x y] b)))))
  rs)

(fn move [p d]
  (if (= d :x)
      p
      (pt+ p (. const.dirdelta d))))

(fn safe-at? [model m p]
  (local k (pk p))
  (fn safe-at-v? [xy]
    (not (?. model k xy (% m (. model xy)))))
  (and (safe-at-v? :x)
       (safe-at-v? :y)))

; To see if the answer is m + 1:
; 1. Test if the end position is safe at m
; 2. DFS towards the start, looking at neighbor positions that are safe at m - i
; 3. If you get there, success

(fn try-m [model start end m]
  (if (< m 0)                      false
      (not (safe-at? model m end)) false
      (pt=? start end)             true
      (do
        (var done false)
        (each [_ d (ipairs [:n :s :w :e :x]) &until done]
          (when (try-m model start (move end d) (- m 1))
                (set done true)))
        done)))

(fn find-m [model start end]
  (var done false)
  (for [m 1 100 1 &until done]
    (print (.. "try " m))
    (when (try-m model start end m)
          (set done m)))
  done)

(fn solve-a [spec]
  (let [model (mkmodel spec)]
    (find-m model [2 1] [(+ model.x 1) (+ model.y 1)])))

(fn solve-b [spec] 0)

(fn check [])

{
  ; :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
