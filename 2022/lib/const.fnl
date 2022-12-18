; lib/const - constants & basic data types

(local a-z "abcdefghijklmnopqrstuvwxyz")
(local A-Z "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(local a-zA-Z (.. a-z A-Z))
(local vowels [:a :e :i :o :u])
(local consonants [:b :c :d :f :g :h :j :k :l :m :n :p :q :r :s :t :v :w :x :y :z])

(local dirs [:up :right :down :left])
(local dirmods [[0 -1] [1 0] [0 1] [-1 0]])
(local dirmods3
  [[-1  0  0] [ 1  0  0]
   [ 0 -1  0] [ 0  1  0]
   [ 0  0 -1] [ 0  0  1]])

{
  : a-z
  : A-Z
  : a-zA-Z
  : vowels
  : consonants

  : dirs
  : dirmods
  : dirmods3
}
