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
  (let [rooms (tbl.map lines read-line)]
    (var r {})
    (each [_ v (ipairs rooms)]
      (tset r v.name v))
    r))

(fn nodes [g]
  (tbl.sorted (icollect [k _ (pairs g)] k)))

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
(fn greedy [g]
  (local dists (floyd-warshall g))
  (local keys (nodes g))

  (var closed-valves {})
  (var steps-left 30)
  (var where :AA)

  (fn candidates []
    (icollect [_ k (ipairs keys)]
      (when (and (> (. g k :flow) 0)
                 (not (. closed-valves k)))
            k)))

  (fn total-future-flow [k]
    (* (. g k :flow)
       (- steps-left (. dists where k) 1)))

  (fn best-candidate []
    (tbl.maxval (candidates) total-future-flow))

  (pretty dists)
  (pretty (best-candidate)))

(fn solve-a [x] (greedy x) 0)
(fn solve-b [x] 0)

{
  : read
  : solve-a
  : solve-b
}
