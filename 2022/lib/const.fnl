; lib/const - constants & basic data types

(local a-z "abcdefghijklmnopqrstuvwxyz")
(local A-Z "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(local a-zA-Z (.. a-z A-Z))

(local dirs [:up :right :down :left])
(local dirmods [[0 -1] [1 0] [0 1] [-1 0]])

{
  : a-z
  : A-Z
  : a-zA-Z
  : dirs
  : dirmods
}
