; 2022/2: Rock Paper Scissors

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(local moves [ :rock :paper :scissors ])
(local movekey { :A :rock :B :paper :C :scissors :X :rock :Y :paper :Z :scissors })
(local reskey { :X :loss :Y :tie :Z :win })
(local shapescore { :rock 1 :paper 2 :scissors 3 })
(local winscore { :loss 0 :tie 3 :win 6 })

(local outcomes
  {
    :rock { :rock :tie :paper :loss :scissors :win }
    :scissors { :rock :loss :paper :win :scissors :tie }
    :paper { :rock :win :paper :tie :scissors :loss }
  })

(fn tomoves [t]
  [(. movekey (. t 1)) (. movekey (. t 2))])

(fn read [x]
  (tbl.map x
    #(str.split $1)))

(fn roundscore [r]
  (let [them (. r 1)
        you (. r 2)]
    (+ (. shapescore you)
       (. winscore (. outcomes you them)))))

(fn solve-a [plays]
  (-> plays
      (tbl.map tomoves)
      (tbl.map roundscore)
      tbl.sum))

(fn findmove [r]
  (let [them (. movekey (. r 1))
        needed (. reskey (. r 2))]
    [them
      (tbl.find moves #(= needed (. outcomes $1 them)))]))

(fn solve-b [plays]
  (-> plays
      (tbl.map findmove)
      (tbl.map roundscore)
      tbl.sum))

{
  : read
  : solve-a
  : solve-b
}
