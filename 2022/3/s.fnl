; 2022/3: Rucksack Reorganization
;
; Both parts of this involve finding a unique element in a set intersection.
; I originally did part a inline in this file, but once I read part b and saw
; that I'd want to intersect three items, I wrote a library to represent sets
; of strings using tables and support intersections on them.
;
; The solutions for part a and b fall pretty neatly out as a result :)

(local sset (require "../lib/sset"))
(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn halves [t]
  (tbl.group t (/ (length t) 2)))

(fn read [lines]
  (tbl.map lines str.explode))

(fn shared [ts]
  (-> ts
      (tbl.map sset.of)
      sset.intersect
      sset.any))

(fn prio [e]
  (fn ord [c] (string.byte c 1))
  (let [ca (ord "a") cz (ord "z")
        cA (ord "A") cZ (ord "Z")]
    (if (and (>= (ord e) ca)
             (<= (ord e) cz))
        (+ (- (ord e) ca) 1)
        (and (>= (ord e) cA)
             (<= (ord e) cZ))
        (+ (- (ord e) cA) 27))))

(fn check []
  (assert (= "c" (shared [["a" "b" "c"] ["c" "d" "e"]])))
  (assert (= 3 (prio "c")))
  (assert (= 29 (prio "C"))))

(fn solve-a [in]
  (-> in
      (tbl.map halves)
      (tbl.map shared)
      (tbl.map prio)
      tbl.sum))

(fn solve-b [in]
  (-> in
      (tbl.group 3)
      (tbl.map shared)
      (tbl.map prio)
      tbl.sum))

{
  : check
  : read
  : solve-a
  : solve-b
}
