; 2022/19: Not Enough Minerals

(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn read [lines]
  (fn precomp [bps]
    (tset bps :max-ore
              (accumulate [m 0 _ v (pairs bps)]
                (if (> v.ore m) v.ore m)))
  )
  (icollect [_ v (ipairs lines)]
    (let [ns (str.allnums v)]
      (var bps
        {
          :ore { :ore (. ns 2) }
          :clay { :ore (. ns 3) }
          :obs { :ore (. ns 4) :clay (. ns 5) }
          :geo { :ore (. ns 6) :obs (. ns 7) }
        })
      (precomp bps)
      bps)))

(fn make-sim [bps mins]
  {
    :bots { :ore 1 :clay 0 :obs 0 :geo 0 }
    :ores { :ore 0 :clay 0 :obs 0 :geo 0 }
    : mins
    : bps
  })

(fn copy [sim]
  {
    :mins sim.mins
    :bots (collect [k v (pairs sim.bots)] (values k v))
    :ores (collect [k v (pairs sim.ores)] (values k v))
    :bps sim.bps
  })

(fn step [sim order]
  (var r (copy sim))
  (tbl.update r :mins #(- $1 1))
  (when order
    (let [cost (. r.bps order)]
      (each [k v (pairs cost)]
        (tbl.update r.ores k #(- $1 v)))))
  (each [k v (pairs sim.bots)]
    (tbl.update r.ores k #(+ $1 v)))
  (when order
    (tbl.update r.bots order #(+ $1 1)))
  r)

; Solution notes:
; 1. I tried dfs, both with no heuristics (search space too big) and with a few
;    heuristics that came to mind, which either produced non-optimal solutions
;    on test cases or did not run fast enough. I did commit it to git and it
;    did at least seem to be working, but I suspect it's unuseably slow and
;    I think my heuristics were not right.
; 2. I tried to write a deterministic best strategy:
;    a. Always build in order: geo, obs, clay, ore..
;    b. Always build the bottleneck for making geos.
;    Neither of these worked on the test inputs. :(
; 3. I tried thinking of this as a zero-one integer linear programming problem,
;    but there are a huge amount of variables: # of bot types * # of steps, and
;    the constraints on build order & resource availability are difficult to
;    express. Also I am pretty sure this would require a gigantic matrix and be
;    quite slow.
;
; So. Kinda stuck. Time for a think.
(fn most-geo [sim] 0)

(fn check []
  (local bps1 {
    :ore { :ore 4 }
    :clay { :ore 2 }
    :obs { :ore 3 :clay 14 }
    :geo { :ore 2 :obs 7 }
    :max-ore 4
  })

  (let [s (step (make-sim bps1 1))]
    (assert-eq s.ores.ore 1)
    (assert-eq s.bots.ore 1))

  (let [s { :bps bps1
            :mins 1
            :ores { :ore 5 :clay 37 :obs 6 :geo 7 }
            :bots { :ore 1 :clay 4 :obs 2 :geo 2 } }]
    (assert-eq (most-geo s) 9))

  (let [s { :bps bps1
            :mins 2
            :ores { :ore 4 :clay 33 :obs 4 :geo 5 }
            :bots { :ore 1 :clay 4 :obs 2 :geo 2 } }]
    (assert-eq (most-geo s) 9))

  (let [s { :bps bps1
            :mins 4
            :ores { :ore 4 :clay 25 :obs 7 :geo 2 }
            :bots { :ore 1 :clay 4 :obs 2 :geo 1 } }]
    (assert-eq (most-geo s) 9))

  (let [s { :bps bps1
            :mins 10
            :ores { :ore 3 :clay 15 :obs 3 :geo 0 }
            :bots { :ore 1 :clay 4 :obs 1 :geo 0 } }]
    (assert-eq (most-geo s) 9))

  (let [s { :bps bps1
            :mins 14
            :ores { :ore 4 :clay 15 :obs 0 :geo 0 }
            :bots { :ore 1 :clay 3 :obs 0 :geo 0 } }]
    (assert-eq (most-geo s) 9))
)

;(fn solve-a [blueprints] (pretty blueprints))
(fn solve-a [blueprints]
  (accumulate [s 0 i v (ipairs blueprints)]
    (let [iv (most-geo (make-sim v 24))]
      (print (.. i ": " iv))
      (+ s (* i iv)))))
(fn solve-b [blueprints] 0)

{
  : check
  : read
  : solve-a
  : solve-b
}
