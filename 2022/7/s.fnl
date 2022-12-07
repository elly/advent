; 2022/7: No Space Left On Device

(local str (require "../lib/str"))
(local tbl (require "../lib/tbl"))

(fn read-cmd [pwd [_ cmd arg]]
  (match [cmd arg]
    ["cd" "/"] []
    ["cd" ".."] (tbl.pop pwd)
    ["cd" x] (tbl.push pwd x)
    _ pwd))

(fn read-ls [fs pwd [sz name]]
  (var t fs)
  (each [_ v (ipairs pwd)]
    (when (not (. t v))
          (tset t v {}))
    (set t (. t v)))
  (tset t name (or (tonumber sz) {})))

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

(fn dirsizes [fs]
  (var r [])
  (fn help [fs]
    (table.insert r (sizeof fs))
    (each [_ v (pairs fs)]
      (when (= (type v) :table)
            (help v))))
  (help fs)
  r)

(fn solve-a [fs]
  (-> fs
      dirsizes
      (tbl.filter (fn [x] (< x 100000)))
      tbl.sum))

(local total 70000000)
(local needed 30000000)

(fn unused [fs] (- total (sizeof fs)))

(fn solve-b [fs]
  (let [mustfind (- needed (unused fs))]
    (-> fs
        dirsizes
        (tbl.filter (fn [x] (>= x mustfind)))
        tbl.sorted
        (. 1))))

{
  : read
  : solve-a
  : solve-b
}
