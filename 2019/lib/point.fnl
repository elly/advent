; lib/point - 2d/3d points

(local *dirs* {
  :n [ 0 -1  0]
  :s [ 0  1  0]
  :w [-1  0  0]
  :e [ 1  0  0]
  :u [ 0  0  1]
  :d [ 0  0 -1]
})

(fn abs [v] (if (< v 0) (* -1 v) v))

(fn add [[x0 y0 z0] [x1 y1 z1]]
  [(+ x0 x1) (+ y0 y1) (+ z0 z1)])

(fn smul [[x y z] v]
  [(* x v) (* y v) (* z v)])

(fn eq? [[x0 y0 z0] [x1 y1 z1]]
  (and (= x0 x1) (= y0 y1) (= z0 z1)))

(fn fromstr [p]
  (icollect [v (p:gmatch "-?%d+")] (tonumber v)))

(fn hat [[x0 y0 z0] [x1 y1 z1]]
  "Return the vector of the smallest magnitude with integral components that
   points from p0 to p1."
  (fn gcd [a b]
    (if (= b 0)
        a
        (gcd b (% a b))))

  (fn gcdz [a b c]
    "Returns the gcd of any nonzero components of [a b c]."
    (match [a b c]
      [0 0 0]          1   ; yuck
      [x 0 0]          x
      [0 y 0]          y
      [0 0 z]          z
      [x y 0]          (gcd x y)
      [x 0 z]          (gcd x z)
      [0 y z]          (gcd y z)
      [x y z]          (gcd (gcd x y) z)))

  (let [dx (- x1 x0) dy (- y1 y0) dz (- z1 z0)
        adx (abs dx) ady (abs dy) adz (abs dz)
        g (gcdz adx ady adz)]
    [(/ dx g) (/ dy g) (/ dz g)]))

(fn make [x y z]
  [x y (or z 0)])

(fn manhattan [[x0 y0 z0] [x1 y1 z1]]
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
    (assert (eq? (fromstr (tostr p)) p) "roundtrip"))

  (let [z  (make 0 0 0)
        p  (make 4 4 2)
        zx (make 10 0 0)
        zy (make 0 10 0)
        zz (make 0 0 10)]
    (assert (eq? (make 2 2 1) (hat z p)))
    (assert (eq? (make -2 -2 -1) (hat p z)))
    (assert (eq? (make 1 0 0) (hat z zx)))
    (assert (eq? (make 0 1 0) (hat z zy)))
    (assert (eq? (make 0 0 1) (hat z zz)))))

{
  : add
  : check
  : eq?
  : fromstr
  : hat
  : make
  : manhattan
  : move
  : smul
  : tostr

  : *dirs*
}
