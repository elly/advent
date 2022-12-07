; 2022/7: No Space Left On Device

(local str (require "../lib/str"))

(fn read-cmd [pwd [_ cmd arg]]
  (match [cmd arg]
    ["cd" "/"] []
    ["cd" ".."] (do (table.remove pwd (length pwd)) pwd)
    ["cd" x] (do (table.insert pwd x) pwd)
    _ pwd))

(fn putfile [t name sz]
  (tset t name (tonumber sz)))

(fn putdir [t name]
  (tset t name {}))

(fn read-ls [fs pwd [sz name]]
  (var t fs)
  (each [_ v (ipairs pwd)]
    (when (not (. t v))
          (putdir t v))
    (set t (. t v)))
  (let [n (tonumber sz)]
    (if n (putfile t name sz)
          (putdir t name))))

(fn read [lines]
  (var fs {})
  (var pwd [])
  (each [_ line (ipairs lines)]
    (let [parts (str.split line)]
      (if (= (. parts 1) "$")
          (set pwd (read-cmd pwd parts))
          (read-ls fs pwd parts))))
  fs)

(fn sizeof [d]
  (if (= (type d) :number)
      d
      (accumulate [s 0 _ e (pairs d)]
        (+ s (sizeof e)))))

(fn walk [fs f]
  (when (= (type fs) :table)
        (f fs)
        (each [_ v (pairs fs)]
          (walk v f))))

(fn small-sizes [fs]
  (var s 0)
  (walk fs
    (fn [e]
      (let [r (sizeof e)]
        (if (< r 100000)
            (set s (+ s r))))))
  s)

(fn solve-a [x] (small-sizes x))

(local total 70000000)
(local needed 30000000)

(fn unused [fs] (- total (sizeof fs)))

(fn smallest-useful [fs n]
  (var best total)
  (walk fs
    (fn [e]
      (let [r (sizeof e)]
        (when (and (>= r n) (< r best))
              (set best r)))))
  best)

(fn solve-b [x]
  (pretty needed)
  (pretty (unused x))
  (smallest-useful x (- needed (unused x))))

{
  : read
  : solve-a
  : solve-b
}
