; lib/intcode: intcode interpreter

(fn fault [vm why state]
  (error (.. vm.tag "fault: " vm.pc " " why ": " (table.concat state ","))))

(fn make-default-in [vm]
  (fn []
    (if (> (# vm.inbuf) 0)
        (table.remove vm.inbuf 1)
        (do (fault vm "noinput" []) 0))))

(fn make-default-out [vm]
  (fn [v]
    (table.insert vm.outbuf v)))

(fn computebin [vm insn a1 a2]
  (match insn.name
    :add (+ a1 a2)
    :mul (* a1 a2)
    _        (fault vm "unimp" [insn.name a1 a2])))

(fn peek [vm addr]
  (when vm.tracing.peek
    (print (.. vm.tag "r " vm.pc ": " addr " <- " (. vm.mem (+ 1 addr)))))
  (or (. vm.mem (+ 1 addr)) 0))

(fn poke [vm addr v]
  (when vm.tracing.poke
    (print (.. vm.tag "w " vm.pc ": " addr " "  (. vm.mem (+ 1 addr)) " -> " v)))
  (tset vm.mem (+ 1 addr) v)
  vm)

(fn argload [vm [val mode]]
  (if (= mode :imm)
      val
      (peek vm val)))

(fn argstore [vm [val mode] v]
  (when (= mode :imm)
        (fault vm "immwrite" [val mode v]))
  (when (= mode :pos)
        (poke vm val v)))

(fn opbin [vm insn [arg1 arg2 arg3]]
  (let [i1 (argload vm arg1)
        i2 (argload vm arg2)]
    (let [v (computebin vm insn i1 i2)]
      (argstore vm arg3 v))))

(fn ophlt [vm insn]
  (tset vm :halt true))

(fn opin [vm insn [arg1]]
  (let [iv (vm.infunc)]
    (assert (or (= nil iv) (= :number (type iv))) "in must yield number or nil")
    (when vm.tracing.io
      (print (.. vm.tag "inp " vm.pc ": " (or iv "(nil)") " -> " (. arg1 1))))
    (if (= nil iv)
      (do (tset vm :iowait true) nil)
      (argstore vm arg1 iv))))

(fn opout [vm insn [arg1]]
  (let [ov (argload vm arg1)]
    (when vm.tracing.io
      (print (.. vm.tag "out " vm.pc ": " (. arg1 1) "/" (. arg1 2) " -> " ov)))
    (vm.outfunc ov)))

(fn jump [vm where]
  (tset vm :pc where))

(fn opjtr [vm insn [arg1 arg2]]
  (when (not (= 0 (argload vm arg1)))
        (jump vm (argload vm arg2))))

(fn opjfa [vm insn [arg1 arg2]]
  (when (= 0 (argload vm arg1))
        (jump vm (argload vm arg2))))

(fn opclt [vm insn [in1 in2 out]]
  (argstore vm out
    (if (< (argload vm in1) (argload vm in2))
        1
        0)))

(fn opceq [vm insn [in1 in2 out]]
  (argstore vm out
    (if (= (argload vm in1) (argload vm in2))
        1
        0)))

(local *ops*
  {
    1 { :name :add :args 3 :fn opbin }
    2 { :name :mul :args 3 :fn opbin }
    3 { :name :inp :args 1 :fn opin }
    4 { :name :out :args 1 :fn opout }
    5 { :name :jtr :args 2 :fn opjtr }
    6 { :name :jfa :args 2 :fn opjfa }
    7 { :name :clt :args 3 :fn opclt }
    8 { :name :ceq :args 3 :fn opceq }
    99 { :name :hlt :args 0 :fn ophlt }
  })

(fn copy [vm]
  (var r {
    :halt vm.halt
    :iowait false
    :inbuf []
    :outbuf []
    :mem (icollect [_ v (ipairs vm.mem)] v)
    :pc vm.pc
    :tag ""
    :tracing (collect [k v (pairs vm.tracing)] k v)
  })
  (tset r :infunc (make-default-in r))
  (tset r :outfunc (make-default-out r))
  r)

(fn make [mem]
  (fn read-mem [ints]
    (icollect [v (ints:gmatch "[^,]+")] (tonumber v)))
  (var mem (if (= (type (. mem 1)) :string)
           (read-mem (. mem 1))
           mem))
  (var r {
    :halt false
    :iowait false
    :inbuf []
    :outbuf []
    : mem
    :pc 0
    :tag ""
    :tracing {}
  })
  (tset r :infunc (make-default-in r))
  (tset r :outfunc (make-default-out r))
  r)

(fn hookio [vm infunc outfunc]
  (when infunc (tset vm :infunc infunc))
  (when outfunc (tset vm :outfunc outfunc))
  vm)

(fn pushin [vm v]
  (table.insert vm.inbuf v)
  vm)

(local *modes* [:pos :imm])

(fn decode [opcode]
  (fn decode-modes [modes]
    (var r [])
    (var modes modes)
    (while (> modes 0)
      (table.insert r (. *modes* (+ 1 (% modes 10))))
      (set modes (math.floor (/ modes 10))))
    r)

  (let [modes (math.floor (/ opcode 100))
        opcode (% opcode 100)]
    (values
      (. *ops* opcode)
      (decode-modes modes))))

(fn halted? [vm] vm.halt)
(fn stopped? [vm]
  (or vm.halt vm.iowait))

(fn step [vm]
  (fn load-args [insn modes]
    (fcollect [i 1 insn.args 1]
      [(peek vm (+ vm.pc i)) (or (. modes i) :pos)]))

  (fn advance-pc [insn]
    (tset vm :pc (+ vm.pc insn.args 1)))

  (fn trace [insn args]
    (local pargs
      (icollect [_ a (ipairs args)]
        (.. (. a 1) "/" (. a 2))))
    (print (.. vm.tag "x " vm.pc ": " insn.name " " (table.concat pargs ", "))))

  (fn dispatch [insn args]
    (when vm.tracing.exec
      (trace insn args))
    (insn.fn vm insn args))

  (let [oldpc vm.pc
        opcode (peek vm vm.pc)
        (insn modes) (decode opcode)]
    (when (not insn)
          (fault vm "badop" [opcode]))
    (dispatch insn (load-args insn modes))
    (when (and (not (stopped? vm)) (= oldpc vm.pc))
          (advance-pc insn)))

  vm)

(fn run [vm]
  (tset vm :iowait false)
  (while (not (stopped? vm))
    (step vm))
  vm)

(fn tag [vm t]
  (tset vm :tag t))

(fn trace [vm modes]
  (tset vm :tracing (collect [_ m (ipairs modes)] (values m true)))
  vm)

{
  : copy
  : halted?
  : hookio
  : make
  : peek
  : poke
  : pushin
  : run
  : step
  : stopped?
  : tag
  : trace
}
