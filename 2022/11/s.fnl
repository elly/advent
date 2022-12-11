; 2022/11: Monkey in the Middle

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

; Monkey operations are [a b c] where old * old * a + old * b + c
(fn read-monkey [lines]
  (var r { :insp 0 })
  (each [_ line (ipairs lines)]
    (match (str.split line)
      ["Starting" _]                 (tset r :items (str.allnums line))
      ["Operation:" _ _ _ "*" "old"] (tset r :op [1 0 0])
      ["Operation:" _ _ _ "*" x]     (tset r :op [0 (tonumber x) 0])
      ["Operation:" _ _ _ "+" x]     (tset r :op [0 1 (tonumber x)])
      ["Test:" _ _ x]                (tset r :mod (tonumber x))
      ["If" "true:" _ _ _ x]         (tset r :t  (+ (tonumber x) 1))
      ["If" "false:" _ _ _ x]        (tset r :f (+ (tonumber x) 1))
      ["Monkey" _]                   nil
        _                            (pretty line)))
  r)

(fn copy-monkeys [monkeys]
  (icollect [_ m (ipairs monkeys)]
    {
      :insp 0
      :items (tbl.acopy m.items)
      :op m.op
      :mod m.mod
      :t m.t
      :f m.f
    }))

(fn inspect [op item rf]
  (math.floor
    (rf
      (+ (* item item (. op 1))
         (* item (. op 2))
         (. op 3)))))

(fn throw-where [m rf]
  (var out [])
  (each [_ item (ipairs m.items)]
    (let [nitem (inspect m.op item rf)]
      (if (= 0 (% nitem m.mod))
          (table.insert out [nitem m.t])
          (table.insert out [nitem m.f]))))
  out)

(fn take-turn! [monkeys i rf]
  (let [outs (throw-where (. monkeys i) rf)]
    (tset monkeys i :insp
      (+ (. monkeys i :insp)
         (length (. monkeys i :items))))
    (tset monkeys i :items [])
    (each [_ out (ipairs outs)]
      (table.insert (. monkeys (. out 2) :items) (. out 1)))))

(fn round! [monkeys rf]
  (for [i 1 (length monkeys) 1]
    (take-turn! monkeys i rf)))

(fn n-rounds! [monkeys n rf]
  (for [i 1 n 1]
    (round! monkeys rf))
  monkeys)

(fn monkey-business [monkeys]
  (-> monkeys
      (tbl.map #(. $1 :insp))
      (tbl.sorted #(> $1 $2))
      (tbl.take 2)
      tbl.prod))

(fn read [lines]
  (-> lines
      (tbl.splitby #(= $1 ""))
      (tbl.map read-monkey)))

(fn check []
  (let [m0 { :insp 0 :items [79 98] :mod 23 :op [0 19 0] :t 3 :f 4 }
        m1 { :insp 0 :items [54 65 75 74] :mod 19 :op [0 1 6] :t 3 :f 1 }
        m2 { :insp 0 :items [79 60 97] :mod 13 :op [1 0 0] :t 2 :f 4 }
        m3 { :insp 0 :items [74] :mod 17 :op [0 1 3] :t 1 :f 2 }
        monkeys [m0 m1 m2 m3]]
    (round! monkeys #(/ $1 3))
    (assert (tbl.aeq m0.items [20 23 27 26]))
    (assert (tbl.aeq m1.items [2080 25 167 207 401 1046]))
    (assert (tbl.aeq m2.items []))
    (assert (tbl.aeq m3.items []))))

(fn solve-a [monkeys]
  (-> monkeys
      copy-monkeys
      (n-rounds! 20 #(/ $1 3))
      monkey-business))

(fn reduce-func [monkeys]
  (let [mod (-> monkeys (tbl.map #(. $1 :mod)) tbl.prod)]
    #(% $1 mod)))

(fn solve-b [monkeys]
  (-> monkeys
      (copy-monkeys)
      (n-rounds! 10000 (reduce-func monkeys))
      monkey-business))

{
;  :debug 1
  : read
  : check
  : solve-a
  : solve-b
}
