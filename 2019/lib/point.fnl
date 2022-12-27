; lib/point - 2d/3d points

(local *dirs* {
  :n [ 0 -1  0]
  :s [ 0  1  0]
  :w [ 1  0  0]
  :e [-1  0  0]
  :u [ 0  0  1]
  :d [ 0  0 -1]
})

(fn add [[x0 y0 z0] [x1 y1 z1]]
  [(+ x0 x1) (+ y0 y1) (+ z0 z1)])

(fn smul [[x y z] v]
  [(* x v) (* y v) (* z v)])

(fn eq? [[x0 y0 z0] [x1 y1 z1]]
  (and (= x0 x1) (= y0 y1) (= z0 z1)))

(fn fromstr [p]
  (icollect [v (p:gmatch "-?%d+")] (tonumber v)))

(fn make [x y z]
  [x y (or z 0)])

(fn manhattan [[x0 y0 z0] [x1 y1 z1]]
  (fn abs [v] (if (< v 0) (* -1 v) v))
  (+ (abs (- x0 x1)) (abs (- y0 y1)) (abs (- z0 z1))))

(fn move [p d n]
  (local n (or n 1))
  (add p (smul (. *dirs* d) n)))

(fn tostr [p]
  (.. (. p 1) "," (. p 2) "," (. p 3)))

(fn check []
  (let [p (make 1 -2 3)
        q (make -2 3 1)]
    (assert (eq? p p) "equality")
    (assert (not (eq? p q)) "inequality")
    (assert (eq? (fromstr (tostr p)) p) "roundtrip")))

{
  : add
  : check
  : eq?
  : fromstr
  : make
  : manhattan
  : move
  : smul
  : tostr

  : *dirs*
}
