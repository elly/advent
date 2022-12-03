; 2022/1: Elf Snack Counting
;
; The input is a set of sets of numbers, one per line, separated by empty lines.
; For part A, we need to find the sum of the set with the largest sum.
; For part B, we need to find the sum of the sums of the three sets with the
; largest sums.

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn snackiest [t]
  (-> t
      (tbl.map tbl.sum)
      (tbl.sorted #(> $1 $2))))

(fn read [lines]
  (-> lines
      (tbl.map str.tonumz)
      (tbl.splitby #(= $1 0))))

(fn solve-a [x]
  (-> (snackiest x)
      (tbl.take 1)
      tbl.sum))

(fn solve-b [x]
  (-> (snackiest x)
      (tbl.take 3)
      tbl.sum))

(fn check []
  (let [in ["10" "20" "" "30" "40" "" "50"]
        r (read in)]
    (assert (tbl.aeq (. r 1) [10 20]))
    (assert (tbl.aeq (. r 2) [30 40]))
    (assert (tbl.aeq (. r 3) [50])))
  (let [t [[1 2 3] [4 5] [7]]
        r (snackiest t)]
    (assert (tbl.aeq r [9 7 6]))))

{
  : read
  : check
  : solve-a
  : solve-b
}
