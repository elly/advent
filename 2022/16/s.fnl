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
      (tset r v.name v))
    r))

(fn nodes [g]
  (tbl.sorted (icollect [k _ (pairs g)] k)))

(fn nonzero-nodes [g]
  (-> (icollect [k _ (pairs g)] k)
      (tbl.filter #(> (. g $1 :flow) 0))
      tbl.sorted))

(fn floyd-warshall [g]
  (var dists {})
  (local keys (nodes g))

  (each [_ v (pairs g)]
    (tset dists v.name {})
    (each [_ k (ipairs keys)]
      (tset dists v.name k 99999999))
    (each [_ e (ipairs v.outs)]
      (tset dists v.name e 1))
    (tset dists v.name v.name 0))

  (each [_ k (ipairs keys)]
    (each [_ i (ipairs keys)]
      (each [_ j (ipairs keys)]
        (when (> (. dists i j) (+ (. dists i k) (. dists k j)))
              (tset dists i j ( + (. dists i k) (. dists k j)))))))

  dists)

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
(fn best-path [g where steps lens opened]
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
          (v p) (best-path g c new-steps lens new-opened)]
      (when (> v best-value)
        (set best-value v)
        (set best-rest p))))

  (set best-value (+ best-value (* (. g where :flow) steps)))
  (table.insert best-rest 1 where)

  ;(pretty [best-value best-rest])
  (values best-value best-rest))

; Part B requires searching a larger space. In particular, we have two
; positions instead of one, and our candidate set is way bigger: we might
; move one of the positions to a room with no flow so we can move the other
; to one with a flow. A candidate is now any movef where *at least one* of
; the positions moves to a room with an unopened valve and nonzero flow. Hm.

(fn solve-a [x] (best-path x :AA 30))
(fn solve-b [x] 0)

{
  : read
  : solve-a
  : solve-b
}
