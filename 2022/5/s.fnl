; 2022/5: Supply Stacks

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn btabl [stacks line]
  (for [i 2 (string.len line) 4]
    (let [ti (/ (+ i 2) 4)
          s (line:sub i i)]
      (if (not (= s " "))
        (do
          (var t (or (. stacks ti) []))
          (table.insert t (line:sub i i))
          (tset stacks ti t)))))
  stacks)

(fn btab [lines]
  (-> lines
      tbl.reverse
      (tbl.drop 1)
      (tbl.fold [] btabl)))

(fn pinstr [instr]
  (let [ps (str.split instr)]
    {
      :n (str.tonumz (. ps 2))
      :from (str.tonumz (. ps 4))
      :to (str.tonumz (. ps 6))
    }))

(fn read [x]
  (let [ps (tbl.splitby x #(= $1 ""))]
    {
      :stacks (btab (. ps 1))
      :instrs (tbl.map (. ps 2) pinstr)
    }))

(fn stkcopy [ss]
  (tbl.map ss tbl.acopy))

(fn doinsn-a [ss in]
  (let [is (. ss in.from)
        os (. ss in.to)]
    (for [i 1 in.n 1]
      (var c (. is (length is)))
      (table.insert os c)
      (table.remove is (length is)))
    (tset ss in.from is)
    (tset ss in.to os))
  ss)

(fn doinsn-b [ss in]
  (let [is (. ss in.from)
        os (. ss in.to)
        d (- (length is) (- in.n 1))]
    (for [i 1 in.n 1]
      (var c (. is d))
      (table.insert os c)
      (table.remove is d))
    (tset ss in.from is)
    (tset ss in.to os))
  ss)

(fn solve [doinsn x]
  (let [ss (stkcopy x.stacks)]
    (-> (tbl.fold x.instrs ss doinsn)
        (tbl.map #(. $1 (length $1)))
        (table.concat ""))))

(fn check []
  (let [in (pinstr "move 3 from 6 to 2")]
    (assert (= 3 in.n))
    (assert (= 6 in.from))
    (assert (= 2 in.to)))
  (let [bt (btabl [[:Z] [:Y] [:X] [:W] [:V]] "[A] [B] [C]     [E]")]
    (assert (tbl.aeq [:Z :A] (. bt 1)))
    (assert (tbl.aeq [:W ] (. bt 4)))
    (assert (tbl.aeq [:V :E] (. bt 5)))))

{
  : read
  : check
  :solve-a #(solve doinsn-a $1)
  :solve-b #(solve doinsn-b $1)
}
