; 2022/11: Monkey in the Middle

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn read-monkey [lines]
  (var r { :insp 0 })
  (each [_ line (ipairs lines)]
    (match (str.split line)
      ["Starting" _]                 (tset r :items (str.allnums line))

      ; Monkey operations are [a b c] where old * old * a + old * b + c
      ["Operation:" _ _ _ "*" "old"] (tset r :op [1 0 0])
      ["Operation:" _ _ _ "*" x]     (tset r :op [0 (tonumber x) 0])
      ["Operation:" _ _ _ "+" x]     (tset r :op [0 1 (tonumber x)])

      ["Test:" _ _ x]                (tset r :mod (tonumber x))
      ["If" "true:" _ _ _ x]         (tset r :t  (+ (tonumber x) 1))
      ["If" "false:" _ _ _ x]        (tset r :f (+ (tonumber x) 1))
      ["Monkey" _]                   nil
        _                            (pretty line)))
  r)

(fn read [lines]
  (-> lines
      (tbl.splitby #(= $1 ""))
      (tbl.map read-monkey)))

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

(fn inspect [op item reduce]
  (math.floor
    (reduce
      (+ (* item item (. op 1))
         (* item (. op 2))
         (. op 3)))))

(fn throw-where [m reduce]
  (var out [])
  (each [_ item (ipairs m.items)]
    (let [nitem (inspect m.op item reduce)]
      (if (= 0 (% nitem m.mod))
          (table.insert out [nitem m.t])
          (table.insert out [nitem m.f]))))
  out)

(fn take-turn! [monkeys i reduce]
  (let [outs (throw-where (. monkeys i) reduce)]
    (tset monkeys i :insp
      (+ (. monkeys i :insp)
         (length (. monkeys i :items))))
    (tset monkeys i :items [])
    (each [_ out (ipairs outs)]
      (table.insert (. monkeys (. out 2) :items) (. out 1)))))

(fn round! [monkeys reduce]
  (for [i 1 (length monkeys) 1]
    (take-turn! monkeys i reduce)))

(fn n-rounds! [monkeys n reduce]
  (for [i 1 n 1]
    (round! monkeys reduce))
  monkeys)

(fn monkey-business [monkeys]
  (-> monkeys
      (tbl.mapkey :insp)
      (tbl.sorted #(> $1 $2))
      (tbl.take 2)
      tbl.prod))

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

(fn solve-b [monkeys]
  ; My key insight for part 2: x % k = (x % (... * k)) % k, so you can constrain
  ; the growth of the worry levels by taking them modulo the product of all the
  ; moduli every time they grow. My math nerd housemate informs me this is the
  ; Chinese Remainder Theorem.
  (let [mod (-> monkeys (tbl.mapkey :mod) tbl.prod)]
    (-> monkeys
        copy-monkeys
        (n-rounds! 10000 #(% $1 mod))
        monkey-business)))

{
  : read
  : check
  : solve-a
  : solve-b
}
