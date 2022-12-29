; 2019/6: Universal Orbit Map

(fn read [lines]
  (fn split [line] ; (
    (icollect [v (line:gmatch "[^)]+")] v))
  (collect [_ line (ipairs lines)]
    (let [[big little] (split line)]
      (values little big))))

(fn objects [t] (icollect [k _ (pairs t)] k))

(fn depth [t a k]
  (var k k)
  (var d 0)
  (while (and k (not (= k a)))
    (set d (+ d 1))
    (set k (. t k)))
  (if k d nil))

(fn solve-a [spec]
  (accumulate [s 0 _ obj (ipairs (objects spec))]
    (+ s (depth spec :COM obj))))

(fn common-ancestor [t a b]
  (var a a)
  (var found false)
  (while (and (not (= a :COM)) (not found))
    (when (depth t a b)
          (set found true))
    (set a (. t a)))
  a)

(fn solve-b [spec]
  (let [ca (common-ancestor spec :YOU :SAN)]
    (+ (depth spec ca :YOU) (depth spec ca :SAN) -4)))

{
  : read
  : solve-a
  : solve-b
}
