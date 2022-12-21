; 2022/21: Monkey Math

(fn read [lines]
  (fn read-monkey [map line]
    (let [(name0 left op right) (line:match "(%a+): (%a+) ([-+/*]) (%a+)")
          (name1 val)           (line:match "(%a+): (-?%d+)")]
      (when name0 (tset map name0 [op left right]))
      (when name1 (tset map name1 (tonumber val)))))
  
  (fn build-tree [m vm k]
    (match (. m k)
      [op left right] [op (build-tree m vm left) (build-tree m vm right)]
      x               (do (tset vm k x) k)))

  (let [map {} vars {}]
    (each [_ m (ipairs lines)]
      (read-monkey map m))
    (let [tree (build-tree map vars :root)]
      [tree vars])))

(fn eval [vars tree]
  (math.floor
    (match tree
      [:+ left right] (+ (eval vars left) (eval vars right))
      [:- left right] (- (eval vars left) (eval vars right))
      [:* left right] (* (eval vars left) (eval vars right))
      [:/ left right] (/ (eval vars left) (eval vars right))
      [:= left right] (if (= (eval vars left) (eval vars right)) 1 0)
      x               (or (. vars x) x))))

(fn num? [v] (= (type v) :number))
(fn str? [v] (= (type v) :string))

(fn reduce [vars tree st]
  (fn table? [v] (= (type v) :table))
  (fn bound? [v] (and (str? v) (. vars v)))
  (fn fixed? [v] (or (bound? v) (num? v)))
  (match tree
    (where [op left right] (and (fixed? left) (fixed? right)))
      (do (tset st :prog true) (eval vars tree))
    [op left right]
      [op (reduce vars left st) (reduce vars right st)]
    (where x (fixed? x))
      (eval vars x)
    _                    tree))

(fn reduce-many [vars tree]
  (var st { :prog true })
  (var t tree)
  (while st.prog
    (tset st :prog false)
    (set t (reduce vars t st)))
  t)

(fn rewrite [lhs rhs]
  (match lhs
    ; ordinary cases
    (where [:+ x y] (num? x)) (values y (- rhs x))
    (where [:+ x y] (num? y)) (values x (- rhs y))
    (where [:- x y] (num? y)) (values x (+ rhs y))
    (where [:* x y] (num? x)) (values y (/ rhs x))
    (where [:* x y] (num? y)) (values x (/ rhs y))
    (where [:/ x y] (num? y)) (values x (* rhs y))

    ; ugly case 1: (= (- k y) c) -> (= (+ k (* -1 y)) c)
    (where [:- x y] (num? x)) (values [:+ x [:* -1 y]] rhs)

    ; ugly case 2: (= (/ k y) c) -> (= k (* c y)) -> (= y (/ k c))
    (where [:/ x y] (num? x)) (values y (/ x rhs))
    _                         (do (pretty lhs) (assert false))))

(fn rewrite-many [t]
  (var lhs (. t 2))
  (var rhs (. t 3))
  (assert (num? rhs))
  (while (not (str? lhs))
    (let [(nl nr) (rewrite lhs rhs)]
      (set lhs nl)
      (set rhs nr)))
  (math.floor rhs))

(fn solve-a [[tree vars]] (reduce-many vars tree))
(fn solve-b [[tree vars]]
  (tset vars :humn nil)
  (tset tree 1 :=)
  (rewrite-many (reduce-many vars tree)))

(fn check []
  (assert-eq 1 (reduce-many { :x 1 } :x))
  (assert-eq 5 (reduce-many { :x 2 :y 3 } [:+ :x :y]))
  (let [t (reduce-many { :x 2 :y 3 } [:+ [:* :x 2] [:+ :z :y]])]
    (assert-eq (. t 2) 4)
    (assert-eq (. t 3 1) :+)
    (assert-eq (. t 3 2) :z)
    (assert-eq (. t 3 3) 3))

  (assert-eq 3 (rewrite-many [:= :x 3]))
  (assert-eq 4 (rewrite-many [:= [:+ :x 1] 5]))
  (assert-eq 4 (rewrite-many [:= [:- 7 :x] 3]))
  (assert-eq 3 (rewrite-many [:= [:/ 12 :x] 4])))

{
  : check
  : read
  : solve-a
  : solve-b
}
