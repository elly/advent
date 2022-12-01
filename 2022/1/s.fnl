; 2022/1: Elf Snack Counting
;
; The input is a set of sets of numbers, one per line, separated by empty lines.
; For part A, we need to find the sum of the set with the largest sum.
; For part B, we need to find the sum of the sums of the three sets with the
; largest sums.

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn snackiest [t]
  (var r (tbl.map t tbl.sum))
  (table.sort r (fn [x y] (> x y)))
  r)

(fn read [lines]
  (tbl.splitby
    (tbl.map lines str.tonumz)
    (fn [x] (= x 0))))

(fn solve-a [x]
  (. (snackiest x) 1))

(fn solve-b [x]
  (tbl.sum (tbl.take (snackiest x) 3)))

(fn check []
  (let [in ["10" "20" "" "30" "40" "" "50"]
        r (read in)]
    (assert (tbl.arrayeq (. r 1) [10 20]))
    (assert (tbl.arrayeq (. r 2) [30 40]))
    (assert (tbl.arrayeq (. r 3) [50])))
  (let [t [[1 2 3] [4 5] [7]]
        r (snackiest t)]
    (assert (tbl.arrayeq r [9 7 6]))))

{
  :read read
  :check check
  :solve-a solve-a
  :solve-b solve-b
}
