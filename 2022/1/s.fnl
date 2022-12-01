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

(fn check []
  (let [in ["10" "20" "" "30" "40" "" "50"]
        r (read in)]
    (assert (= (. r 1 1) 10)
            (= (. r 1 2) 20)
            (= (. r 2 1) 30)
            (= (. r 2 2) 40)
            (= (. r 3 1) 50)))
  (let [t [[1 2 3] [4 5] [7]]
        r (snackiest t)]
    (assert (= (. r 1) 9))
    (assert (= (. r 2) 7))
    (assert (= (. r 3) 6))))

(fn solve-a [x]
  (. (snackiest x) 1))

(fn solve-b [x]
  (tbl.sum (tbl.take (snackiest x) 3)))

{
  :read read
  :check check
  :solve-a solve-a
  :solve-b solve-b
}
