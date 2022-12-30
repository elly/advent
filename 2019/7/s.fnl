; 2019/7: Amplification Circuit

(local ic (require :../lib/intcode))
(local list (require :../lib/list))

(fn run-all-until-halted [vms fo]
  (fn spin []
    (var all-halted true)
    (each [_ vm (ipairs vms)]
      (when (not (ic.halted? vm))
        (ic.run vm)
        (set all-halted (and all-halted (ic.halted? vm)))))
    all-halted)

  (while (not (spin)))

  (. fo (# fo)))

(fn make-amp [image loopback inits]
  (var final-out [])
  (fn make-pipe [inits tee]
    (var buf (icollect [_ v (ipairs inits)] v))
    (values
      (fn [] (table.remove buf 1))
      (fn [v]
        (table.insert buf v)
        (when tee (table.insert tee v)))))

  (fn plug-together [v1 v2 iv tee]
    (let [(in out) (make-pipe iv tee)]
      (ic.hookio v1 nil out)
      (ic.hookio v2 in nil)))

  (local a (ic.copy image))
  (local b (ic.copy image))
  (local c (ic.copy image))
  (local d (ic.copy image))
  (local e (ic.copy image))

  (ic.tag a "a ")
  (ic.tag b "b ")
  (ic.tag c "c ")
  (ic.tag d "d ")
  (ic.tag e "e ")

  (plug-together a b [(. inits 2)])
  (plug-together b c [(. inits 3)])
  (plug-together c d [(. inits 4)])
  (plug-together d e [(. inits 5)])
  (if loopback
    (plug-together e a [(. inits 1) 0] final-out)
    (do
      (ic.pushin a (. inits 1))
      (ic.pushin a 0)
      (set final-out e.outbuf)))

  (values [a b c d e] final-out))

(fn try-amp [image loopback inits]
  (let [(amp fo) (make-amp image loopback inits)]
    (run-all-until-halted amp fo)))

(fn solve-a [image]
  (let [settings (list.permutations [0 1 2 3 4])]
    (let [(k v) (list.maximize settings #(try-amp image false $1))]
      v)))

(fn solve-b [image]
  (let [settings (list.permutations [5 6 7 8 9])]
    (let [(k v) (list.maximize settings #(try-amp image true $1))]
      v)))

{
  :read ic.make
  : solve-a
  : solve-b
}
