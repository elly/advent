; 2019/2: 1202 Program Alarm

(local ic (require :../lib/intcode))

(fn run-with-vals [image noun verb]
  (-> image
      ic.copy
      (ic.poke 1 noun)
      (ic.poke 2 verb)
      ic.run
      (ic.peek 0)))

(fn solve-a [image]
  (run-with-vals image 12 2))

(fn solve-b [image]
  (var found nil)
  (for [noun 0 99 1]
    (for [verb 0 99 1]
      (when (= 19690720 (run-with-vals image noun verb))
            (set found [noun verb]))))
  (if found
    (+ (* 100 (. found 1)) (. found 2))
    0))

{
  :read ic.make
  : solve-a
  : solve-b
}
