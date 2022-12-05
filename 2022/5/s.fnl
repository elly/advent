; 2022/5: Supply Stacks

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn stkcopy [ss]
  (tbl.map ss tbl.acopy))

(fn btabl [stacks line]
  (let [ps (tbl.group (str.explode line) 4)]
    (each [i v (ipairs ps)]
      (if (not (= " " (. v 2)))
          (table.insert (. stacks i) (. v 2))))))

(fn btab [lines]
  (let [ls (tbl.reverse lines)
        h (. (tbl.take ls 1) 1)
        t (tbl.drop ls 1)]
    (var r
      (tbl.map (str.allmatches h "%d+")
               (fn [_] [])))
    (each [_ v (ipairs t)]
      (btabl r v))
    r))

(fn pinstr [instr]
  (let [ns (str.allmatches instr "%d+")]
    {
      :n (str.tonumz (. ns 1))
      :from (str.tonumz (. ns 2))
      :to (str.tonumz (. ns 3))
    }))

(fn read [x]
  (let [ps (tbl.splitby x #(= $1 ""))]
    {
      :stacks (btab (. ps 1))
      :instrs (tbl.map (. ps 2) pinstr)
    }))

(fn idxmap-a [s in it] (length s))
(fn idxmap-b [s in it] (+ (- (length s) (- in.n 1)) (- it 1)))

(fn doinsn [ss in f]
  (for [i 1 in.n 1]
    (let [d (f (. ss in.from) in i)]
      (table.insert (. ss in.to) (. ss in.from d))
      (table.remove (. ss in.from) d)))
  ss)

(fn solve [f x]
  (let [ss (stkcopy x.stacks)]
    (-> (tbl.fold x.instrs ss #(doinsn $1 $2 f))
        (tbl.map #(. $1 (length $1)))
        (table.concat ""))))

(fn check []
  (let [in (pinstr "move 3 from 6 to 2")]
    (assert (= 3 in.n))
    (assert (= 6 in.from))
    (assert (= 2 in.to)))
  (let [bt [[:Z] [:Y] [:X] [:W] [:V]]]
    (btabl bt "[A] [B] [C]     [E]")
    (assert (tbl.aeq [:Z :A] (. bt 1)))
    (assert (tbl.aeq [:W ] (. bt 4)))
    (assert (tbl.aeq [:V :E] (. bt 5)))))

{
  : read
  : check
  :solve-a #(solve idxmap-a $1)
  :solve-b #(solve idxmap-b $1)
}
