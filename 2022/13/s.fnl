; 2022/13

(local tbl (require "../lib/tbl"))

(fn readval-inner [line]
  (var in line)

  (fn head [] (string.sub in 1 1))
  (fn eat [c]
    (assert (= c (string.sub in 1 1)))
    (set in (string.sub in 2)))
  (fn maybe-eat [c]
    (when (= (head) c)
          (eat c)))

  (if (= (head in) "[")
      (do
        (eat "[")
        (var r [])
        (while (not (= (head in) "]"))
               (let [(val rest) (readval-inner in)]
                 (table.insert r val)
                 (set in rest))
               (maybe-eat ","))
        (eat "]")
        (values r in))
      (let [(start end) (string.find in "%d+")]
        (values
          (tonumber (string.sub in start end))
          (string.sub in (+ end 1))))))

(fn readval [line]
  (pick-values 1 (readval-inner line)))

(fn read [lines]
  (-> lines
      (tbl.filter #(not (= $1 "")))
      (tbl.map readval)))

(fn compare [t0 t1]
  (fn icompare [i0 i1]
    (if
      (< i0 i1) :less
      (> i0 i1) :greater
                :equal))

  (fn tcompare [t0 t1]
    (var r :equal)
    (var i 1)
    (while (and (= r :equal) (<= i (# t0)) (<= i (# t1)))
      (set r (compare (. t0 i) (. t1 i)))
      (set i (+ i 1)))
    (when (= r :equal)
      (if (< (# t0) (# t1)) (set r :less)
          (> (# t0) (# t1)) (set r :greater)))
    r)                   

  (match [(type t0) (type t1)]
    [:number :number]  (icompare t0 t1)
    [:number :table]   (compare [t0] t1)
    [:table :number]   (compare t0 [t1])
    [:table :table]    (tcompare t0 t1)))

(fn solve-a [ps]
  (accumulate [s 0
               i p (ipairs (tbl.group ps 2))]
    (if (= (compare (. p 1) (. p 2)) :less)
        (+ s i)
        s)))

(fn solve-b [ps]
  (fn isdivm2 [e] (match e [[2]] true))
  (fn isdivm6 [e] (match e [[6]] true))
  (table.insert ps [[2]])
  (table.insert ps [[6]])
  (table.sort ps #(= (compare $1 $2) :less))
  (* (tbl.indexf ps isdivm2)
     (tbl.indexf ps isdivm6)))

(fn check []
  (assert-eq 1 (readval "1"))
  (assert (tbl.aeq [1] (readval "[1]")))
  (assert (tbl.aeq [1 2 3] (readval "[1,2,3]")))
  (let [r (readval "[1,[2,3],4]")]
    (assert (tbl.aeq (. r 2) [2 3]))
    (assert-eq (. r 1) 1)
    (assert-eq (. r 3) 4))

  (assert-eq :less (compare [1 1 3 1 1] [1 1 5 1 1]))
  (assert-eq :less (compare [[1] [2 3 4]] [[1] 4]))
  (assert-eq :greater (compare [9] [[8 7 6]]))
  (assert-eq :less (compare [[4 4] 4 4] [[4 4] 4 4 4]))
  (assert-eq :greater (compare [7 7 7 7] [7 7 7]))
  (assert-eq :less (compare [] [3])))

{
;  :debug 1
  : check
  : read
  : solve-a
  : solve-b
}
