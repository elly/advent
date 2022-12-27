; lib/intcode: intcode interpreter

(fn fault [vm why state]
  (error (.. "fault: " vm.pc " " why ": " (table.concat state ","))))

(fn computebin [vm insn a1 a2]
  (match insn.name
    :add (+ a1 a2)
    :mul (* a1 a2)
    _        (fault vm "unimp" [insn.name a1 a2])))

(fn peek [vm addr]
  (when vm.tracing.peek
    (print (.. "r " vm.pc ": " addr " <- " (. vm.mem (+ 1 addr)))))
  (or (. vm.mem (+ 1 addr)) 0))

(fn poke [vm addr v]
  (when vm.tracing.poke
    (print (.. "w " vm.pc ": " addr " "  (. vm.mem (+ 1 addr)) " -> " v)))
  (tset vm.mem (+ 1 addr) v)
  vm)

(fn argload [vm arg]
  (peek vm arg))

(fn argstore [vm arg v]
  (poke vm arg v))

(fn opbin [vm insn [arg1 arg2 arg3]]
  (let [i1 (argload vm arg1)
        i2 (argload vm arg2)]
    (let [v (computebin vm insn i1 i2)]
      (argstore vm arg3 v))))

(fn ophlt [vm insn]
  (tset vm :halt true))

(local *ops*
  {
    1 { :name :add :args 3 :fn opbin }
    2 { :name :mul :args 3 :fn opbin }
    99 { :name :hlt :args 0 :fn ophlt }
  })

(fn copy [vm]
  {
    :halt vm.halt
    :mem (icollect [_ v (ipairs vm.mem)] v)
    :pc vm.pc
    :tracing (collect [k v (pairs vm.tracing)] k v)
  })

(fn make [mem]
  (fn read-mem [ints]
    (icollect [v (ints:gmatch "[^,]+")] (tonumber v)))
  (var mem (if (= (type (. mem 1)) :string)
           (read-mem (. mem 1))
           mem))
  {
    :halt false
    : mem
    :pc 0
    :tracing {}
  })

(fn decode [opcode] (. *ops* opcode))

(fn step [vm]
  (fn load-args [insn]
    (fcollect [i 1 insn.args 1]
      (peek vm (+ vm.pc i))))

  (fn advance-pc [insn]
    (tset vm :pc (+ vm.pc insn.args 1)))

  (fn trace [insn args]
    (print (.. "x " vm.pc ": " insn.name " " (table.concat args ", "))))

  (fn dispatch [insn args]
    (when vm.tracing.exec
      (trace insn args))
    (insn.fn vm insn args))

  (let [oldpc vm.pc
        opcode (peek vm vm.pc)
        insn (decode opcode)]
    (when (not insn)
          (fault vm "badop" [opcode]))
    (dispatch insn (load-args insn))
    (when (= oldpc vm.pc)
          (advance-pc insn)))

  vm)

(fn run [vm]
  (while (not vm.halt)
    (step vm))
  vm)

(fn trace [vm modes]
  (tset vm :tracing (collect [_ m (ipairs modes)] (values m true)))
  vm)

{
  : copy
  : make
  : peek
  : poke
  : run
  : step
  : trace
}
