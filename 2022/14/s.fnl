; 2022/14

(local points (require :../lib/points))
(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn point+ [p0 p1]
  [(+ (. p0 1) (. p1 1)) (+ (. p0 2) (. p1 2))])

(fn point=? [p0 p1]
  (and (= (. p0 1) (. p1 1))
       (= (. p0 2) (. p1 2))))

(fn point->str [p]
  (.. (. p 1) "," (. p 2)))

(fn read [lines]
  (var level { :lowwall 0 })

  (fn unit1 [v]
    (if (< v 0) -1
        (> v 0)  1
                 0))

  (fn unit [start end]
    (let [dx (- (. end 1) (. start 1))
          dy (- (. end 2) (. start 2))]
      [(unit1 dx) (unit1 dy)]))

  (fn mark-wall [p]
    (tset level (point->str p) "#")
    (when (> (. p 2) level.lowwall)
          (tset level :lowwall (. p 2))))

  (fn read-line-segment [start end]
    (let [sp (str.allnums start)
          ep (str.allnums end)
          u (unit sp ep)]
      (var p sp)
      (while (not (point=? p ep))
        (mark-wall p)
        (set p (point+ p u)))
      (mark-wall p)))

  (fn read-line [line]
    (let [parts (tbl.filter (str.split line) #(not (= $1 "->")))]
      (var start (. parts 1))
      (var rest (tbl.drop parts 1))
      (while (> (# rest) 0)
        (read-line-segment start (. rest 1))
        (set start (. rest 1))
        (set rest (tbl.drop rest 1)))))

  (each [_ line (ipairs lines)]
    (read-line line))
  (tset level :lowwall (+ level.lowwall 2))
  level)

(fn copy-level [level]
  (var nlevel {})
  (each [k v (pairs level)]
    (tset nlevel k v))
  nlevel)

(fn step [level sand]
  (fn empty? [p]
    (and (not (. level (point->str p)))
         (not (= (. p 2) level.stopat))))

  (local DOWN [0 1])
  (local DOWNLEFT [-1 1])
  (local DOWNRIGHT [1 1])

  (if (empty? (point+ sand DOWN))      (point+ sand DOWN)
      (empty? (point+ sand DOWNLEFT))  (point+ sand DOWNLEFT)
      (empty? (point+ sand DOWNRIGHT)) (point+ sand DOWNRIGHT)
      sand))

(fn drop [level]
  (var sand [500 0])
  (var moving true)
  (var abyss false)
  (while (and moving (not abyss))
    (let [nsand (step level sand)]
      (when (point=? sand nsand)
        (set moving false))
      (when (> (. nsand 2) level.lowwall)
        (set abyss true))
      (set sand nsand)))
  (when (not abyss)
    (tset level (point->str sand) "o"))
  (not abyss))

(fn solve-a [level]
  (var lev (copy-level level))
  (var d 0)
  (while (drop lev)
    (set d (+ d 1)))
  d)

(fn solve-b [level]
  (var lev (copy-level level))
  (tset lev :stopat level.lowwall)
  (var d 0)
  (while (not (. lev "500,0"))
    (drop lev)
    (set d (+ d 1)))
  d)

{
  : read
  : solve-a
  : solve-b
}
