; 2019/5: Sunny with a Chance of Asteroids

(local ic (require :../lib/intcode))

(fn solve-a [vm]
  (let [vm (ic.copy vm)]
    (ic.pushin vm 1)
    (ic.run vm)
    (. vm.outbuf (# vm.outbuf))))

(fn solve-b [vm]
  (let [vm (ic.copy vm)]
    (ic.pushin vm 5)
    (ic.run vm)
    (. vm.outbuf (# vm.outbuf))))

{
  :read ic.make
  : solve-a
  : solve-b
}
