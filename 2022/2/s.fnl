; 2022/2: Rock Paper Scissors

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn tomove [x]
  (match x
    :A :rock :X :rock
    :B :paper :Y :paper
    :C :scissors :Z :scissors))

(fn toresult [x]
  (match x
    :X :loss
    :Y :tie
    :Z :win))

(fn read [x]
  (tbl.map x
    #(str.split $1)))

(fn outcome [p o]
  (match [p o]
    [:rock :rock] :tie
    [:rock :paper] :loss
    [:rock :scissors] :win
    [:paper :paper] :tie
    [:paper :scissors] :loss
    [:paper :rock] :win
    [:scissors :scissors] :tie
    [:scissors :rock] :loss
    [:scissors :paper] :win))

(fn shapescore [p]
  (match p
    :rock 1
    :paper 2
    :scissors 3))

(fn winscore [w]
  (match w
    :loss 0
    :tie 3
    :win 6))

(fn roundscore [r]
  (let [them (. r 1)
        you (. r 2)]
    (+ (shapescore you)
       (winscore (outcome you them)))))

(fn solve-a [plays]
  (-> plays
      (tbl.map #(tbl.map $1 tomove))
      (tbl.map roundscore)
      tbl.sum))

(fn findmove [r]
  (let [them (tomove (. r 1))
        needed (toresult (. r 2))]
    (var z nil)
    (each [_ m (ipairs [:rock :paper :scissors])]
      (if (= needed (outcome m them))
          (set z m)))
    [them z]))

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
