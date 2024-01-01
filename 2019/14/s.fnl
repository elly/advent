; 2019/14: Space Stoichiometry

(fn ore-depth [rules t]
  (if (= t :ORE)
    0
    (+ 1
      (accumulate [m -1 _ i (ipairs (. rules t :in))]
        (let [[q e] i
              d (* q (ore-depth rules e))]
          (if (> d m) d m))))))

(fn read [lines]
  (fn read-qty [str]
    [(tonumber ((str:gmatch "%d+"))) ((str:gmatch "[A-Z]+"))])

  (fn read-line [line]
    (let [vals (icollect [v (line:gmatch "%d+ [A-Z]+")] (read-qty v))
          r (table.remove vals (# vals))]
      (values (. r 2) { :in vals :out r })))

  (local rules (collect [_ line (ipairs lines)] (read-line line)))
  (local elems (icollect [k _ (pairs rules)] k))
  (local edepths (collect [k _ (pairs rules)] (values k (ore-depth rules k))))

  (table.sort elems #(> (. edepths $1) (. edepths $2)))
  (icollect [_ k (pairs elems)] (. rules k)))

; Some thinking notes: a recursive solution won't work, because the "arms"
; aren't necessarily independent. For example, if we had:
;   3 ORE => 3 A
;   1 A => 1 B
;   1 A => 1 C
;   1 B, 1 C => 1 FUEL
; If we searched both arms separately we'd wind up making too much of A (3 for
; one arm, 3 for the other).
;
; Instead, we need to search through the space of possible things we can make -
; from any given state, there are edges to all the states that we have enough
; resources to make. We are looking to minimize total ore cost, so we can keep
; track of the best ore cost we've seen so far to get rid of branches. Can we
; also track a visited set, in terms of resources + cost?
;
; Also, I think we want to try things in topological sort order - meaning,
; start from recipies that yield fuel, then its immediate dependencies, then
; go from there. I think we actually need to sort them by how far they are from
; ore?

(fn search [spec 

{
  :debug 1
  : read
}
