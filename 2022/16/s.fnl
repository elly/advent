; 2022/16: Proboscidea Volcanium

(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn read [lines]
  (fn read-line [line]
    (let [places (icollect [s (line:gmatch "[A-Z][A-Z]")] s)
          flow ((line:gmatch "%d+"))]
      {
        :name (. places 1)
        :outs (tbl.drop places 1)
        :flow (tonumber flow)
      }))

  (var r {})
  (let [rooms (tbl.map lines read-line)]
    (each [_ v (ipairs rooms)]
      (tset r v.name v)
      (tset v :name nil))
    r))

(fn nodes [g]
  (tbl.sorted (icollect [k _ (pairs g)] k)))

(fn nonzero-nodes [g]
  (-> (icollect [k _ (pairs g)] k)
      (tbl.filter #(> (. g $1 :flow) 0))
      tbl.sorted))

(fn floyd-warshall [g]
  (var dists {})
  (var nexts {})
  (local keys (nodes g))

  (each [vn v (pairs g)]
    (tset dists vn {})
    (tset nexts vn {})
    (each [_ k (ipairs keys)]
      (tset dists vn k 99999999))
    (each [_ e (ipairs v.outs)]
      (tset dists vn e 1)
      (tset nexts vn e e))
    (tset dists vn vn 0)
    (tset nexts vn vn 0))

  (each [_ k (ipairs keys)]
    (each [_ i (ipairs keys)]
      (each [_ j (ipairs keys)]
        (when (> (. dists i j) (+ (. dists i k) (. dists k j)))
              (tset dists i j ( + (. dists i k) (. dists k j)))
              (tset nexts i j (. nexts i k))))))

  (values dists nexts))

(fn fw-path [nexts s e]
  (var p [s])
  (var n s)
  (while (not (= n e))
    (set n (. nexts n e))
    (table.insert p n))
  p)

; The greedy algorithm looks like this: always choose the node with the largest
; remaining time after reaching * flow rate. To do that, we need the distance
; map the above Floyd-Warshall implementation builds, plus a set of opened
; valves so we can pick a closed one.
;
; I tried this on the test input and it chooses JJ first, not DD; DD is at
; distance 1 so we get (* 28 20), while JJ is at distance 2 so we get
; (* 27 21) for 567. Since the problem statement says DD is the optimal first
; choice, this algorithm won't work. Hmb.
;
; We might need to do backtracking search instead of some sort, but I'm afraid
; of how big the search space is: there are 15! permutations of the nonzero
; flow nodes in my input, which is a Lot. I might still need some heuristics.
;
; Although... the actual distances are large sometimes, so many of the paths
; are excluded. I wonder if we can do dfs, stopping when we run out of steps,
; and hunt for a max that way?

(fn value-of-path [g d p]
  (var steps 30)
  (var value 0)
  (var flow 0)
  (var where :AA)

  (each [_ e (ipairs p) &until (<= steps 0)]
    (let [used (+ 1 (. d where e))]
      (set steps (- steps used))
      (set value (+ value (* flow used)))
      (set flow (+ flow (. g e :flow)))
      (set where e)))
  (+ value (* steps flow)))

; This approach does work for part A, and doesn't take that long. Yay!
(fn best-path-a [g where steps lens opened]
  (local lens
    (or lens
      {
        :dists (floyd-warshall g)
        :vkeys (nonzero-nodes g)
        :keys (nodes g)
      }))
  (local opened (or opened {}))

  (fn candidates []
    (icollect [_ k (ipairs lens.vkeys)]
      (when (and (not (. opened k))
                 (< (. lens :dists where k) steps))
            k)))

  (fn add-opened [n]
    (var copy (collect [k _ (pairs opened)] (values k true)))
    (tset copy n true)
    copy)

  (var best-value 0)
  (var best-rest [])
  (each [_ c (ipairs (candidates))]
    (let [new-steps (- steps (. lens :dists where c) 1)
          new-opened (add-opened c)
          (v p) (best-path-a g c new-steps lens new-opened)]
      (when (> v best-value)
        (set best-value v)
        (set best-rest p))))

  (set best-value (+ best-value (* (. g where :flow) steps)))
  (table.insert best-rest 1 where)

  ;(pretty [best-value best-rest])
  (values best-value best-rest))

(fn solve-a [x] (best-path-a x :AA 30))

; A bit of a green-field approach for part B. The path elements are now
; pairs (p,q) of places where the two pawns move, and the pawns don't
; necessarily open when they move. That causes a huge explosion in the state
; space, because when p moves (taking s steps), we consider *every* node within
; s steps of q, and vice versa. That's obviously going to be too big to be
; workable.
;
; To keep the state space down, let's do this: when we're considering moving p,
; we should only consider moves of q that would take it towards a goal node,
; and we should only consider moves that take it as far as possible towards
; that goal. That means that if we're moving p 4 spaces and q has a goal node
; 5 spaces away, we would *only* consider moving q to that node 4 spaces closer
; to the goal. We would have to figure out what that node is, but maybe we can
; Floyd-Warshall that too...

(fn check []
  (let [g { :AA { :flow 0 :outs [ :BB :CC ] }
            :BB { :flow 1 :outs [ :AA :DD ] }
            :CC { :flow 2 :outs [ :AA :EE ] }
            :DD { :flow 1 :outs [ :BB ] }
            :EE { :flow 0 :outs [ :CC ] }}
        (ds ps) (floyd-warshall g)]
    (assert-eq 2 (. ds :AA :DD))
    (assert-eq 4 (. ds :DD :EE))
    (assert (tbl.aeq [:EE :CC :AA :BB :DD] (fw-path ps :EE :DD)))))

; This absolutely should not work, and on the test input it does not work. I
; can't really believe it produces the right answer on the production input.
; All we do is, we run the algorithm from part A once, then *delete those
; nodes' values*, then run the algorithm again. The only way this can work is
; if the second pawn would never open something earlier than the first pawn.
;
; I have no idea.
(fn best-path-b [g wp wq steps lens opened]
  (local lens
    (or lens
      (let [(ds ns) (floyd-warshall g)]
        {
          :dists ds
          :nexts ns
          :vkeys (nonzero-nodes g)
        })))
  (local opened (or opened {}))

  (let [(v0 p0) (best-path-a g wp steps lens opened)]
    (each [_ k (ipairs p0)]
      (tset g k :flow 0))
    (let [(v1 p1) (best-path-a g wq steps lens opened)]
      (+ v0 v1))))

(fn solve-b [g] (best-path-b g :AA :AA 26))

{
  : check
  : read
  : solve-a
  : solve-b
}
