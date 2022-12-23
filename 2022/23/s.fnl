; 2022/23: Unstable Diffusion

(local const (require :../lib/const))
(local str (require :../lib/str))

(local *elf* :#)

(fn pk [[x y]] (.. x "/" y))
(fn unpk [k] (str.allnums k))

(fn pt+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(fn read [lines]
  (var r {})
  (each [y line (ipairs lines)]
    (each [x char (ipairs (str.explode line))]
      (when (= char *elf*)
            (tset r (pk [x y]) *elf*))))
  r)

(fn tile [m e d]
  (let [np (pt+ e (or (. const.dirdelta d) [0 0]))]
    (. m (pk np))))

(fn empty? [m e d] (not (= (tile m e d) *elf*)))

(fn no-neighbors? [m e]
  (accumulate [n true dn _ (pairs const.dirdelta) &until (not n)]
    (empty? m e dn)))

(local *try-fns*
  {
    :done (fn [m e] (and (no-neighbors? m e) :x))
    :n (fn [m e] (and (empty? m e :nw) (empty? m e :n) (empty? m e :ne) :n))
    :s (fn [m e] (and (empty? m e :sw) (empty? m e :s) (empty? m e :se) :s))
    :w (fn [m e] (and (empty? m e :nw) (empty? m e :w) (empty? m e :sw) :w))
    :e (fn [m e] (and (empty? m e :ne) (empty? m e :e) (empty? m e :se) :e))
    :stuck (fn [m e] :x)
  })

(fn propose [map elf dirs]
  (accumulate [p nil _ dir (ipairs dirs) &until p]
    ((. *try-fns* dir) map (unpk elf))))

(fn proposals [map dirs]
  (fn move [e d]
    (if (= d :x)
        e
        (pk (pt+ (unpk e) (. const.dirdelta d)))))
  (local dirs [:done (. dirs 1) (. dirs 2) (. dirs 3) (. dirs 4) :stuck])
  (collect [elf _ (pairs map)]
    (values elf (move elf (propose map elf dirs)))))

(fn count-elves [map]
  (accumulate [n 0 _ _ (pairs map)]
    (+ n 1)))

(fn move-elves [map proposals]
  (fn avoid-collisions [proposals]
    (var destcount {})
    (each [_ dest (pairs proposals)]
      (tset destcount dest (+ 1 (or (. destcount dest) 0))))
    (collect [src dest (pairs proposals)]
      (if (= (. destcount dest) 1)
          (values src dest)
          (values src src))))

  (collect [_ dest (pairs (avoid-collisions proposals))]
    dest *elf*))

(fn any-moves? [proposals]
  (accumulate [any false k v (pairs proposals) &until any]
    (not (= k v))))

(fn round [map dirs]
  (var ndirs (icollect [_ v (ipairs dirs)] v))
  (table.insert ndirs (table.remove ndirs 1))
  (let [ps (proposals map dirs)]
    (values (move-elves map ps) ndirs (any-moves? ps))))

(fn bounding-box [map]
  (let [points (icollect [k _ (pairs map)] (unpk k))
        xs (icollect [_ v (ipairs points)] (. v 1))
        ys (icollect [_ v (ipairs points)] (. v 2))]
    (table.sort xs)
    (table.sort ys)
    [(. xs 1) (. ys 1) (. xs (# xs)) (. ys (# ys))]))

(fn solve-a [map]
  (var m map)
  (var ds [:n :s :w :e])
  (for [i 1 10 1]
    (let [(nm nds _) (round m ds)]
      (set m nm)
      (set ds nds)))
  (let [bb (bounding-box m)]
    (-
      (* (- (. bb 3) (. bb 1) -1)
         (- (. bb 4) (. bb 2) -1))
      (count-elves m))))

(fn solve-b [map]
  (var m map)
  (var ds [:n :s :w :e])
  (var rn 0)
  (var done false)
  (while (not done)
    (let [(nm nds moved) (round m ds)]
      (set m nm)
      (set ds nds)
      (set done (not moved))
      (set rn (+ rn 1))))
  rn)

(fn check [])

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
