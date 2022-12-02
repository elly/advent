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

(fn tomoves [[a b]]
  [(. movekey a) (. movekey b)])

(fn read [x]
  (tbl.map x
    #(str.split $1)))

(fn roundscore [[them you]]
  (+ (. shapescore you)
     (. winscore (. outcomes you them))))

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

(fn check []
  (assert (= 8 (roundscore [:rock :paper])))
  (assert (= 1 (roundscore [:paper :rock])))
  (assert (= 6 (roundscore [:scissors :scissors])))

  (assert (tbl.aeq [:rock :rock] (findmove [:A :Y])))
  (assert (tbl.aeq [:paper :rock] (findmove [:B :X])))
  (assert (tbl.aeq [:scissors :rock] (findmove [:C :Z]))))

{
  : check
  : read
  : solve-a
  : solve-b
}
