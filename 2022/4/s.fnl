; 2022/4: Camp Cleanup

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn read [x]
  (tbl.map x
    #(tbl.map (icollect [v ($1:gmatch "[^-,]+")] v) str.tonumz)))

(fn contained [[a b c d]]
  (or (and (<= a c) (>= b d))
      (and (<= c a) (>= d b))))

(fn solve-a [x]
  (-> x
      (tbl.filter contained)
      length))

(fn overlapping [[a b c d]]
  (or (contained [a b c d])
      (and (>= a d) (<= b c))
      (and (>= d a) (<= c b))))

(fn solve-b [x]
  (-> x
      (tbl.filter overlapping)
      length))

(fn check []
  (assert (contained [2 8 3 7]))
  (assert (contained [4 6 6 6]))
  (assert (not (contained [2 4 6 8])))

  (assert (overlapping [5 7 7 9]))
  (assert (overlapping [2 8 3 7]))
  (assert (overlapping [2 6 4 8])))

{
  : read
  : check
  : solve-a
  : solve-b
}
