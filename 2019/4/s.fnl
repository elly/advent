; 2019/4: Secure Container

(fn read [[min max]]
  [(tonumber min) (tonumber max)])

(fn digits [n]
  (var r [])
  (var n n)
  (while (> n 0)
    (table.insert r 1 (% n 10))
    (set n (math.floor (/ n 10))))
  r)

(fn digit-deltas [num]
  (let [ds (digits num)]
    (fcollect [i 1 (- (# ds) 1) 1]
      (- (. ds (+ i 1)) (. ds i)))))

(fn digit-runs [num]
  (var runs [])
  (var rc 0)
  (var last nil)
  (let [ds (digits num)]
    (each [_ d (ipairs ds)]
      (when (not (= d last))
        (when (> rc 0)
              (table.insert runs rc))
        (set last d)
        (set rc 0))
      (set rc (+ rc 1))))
  (when (> rc 0)
        (table.insert runs rc))
  runs)

(fn has? [list pred]
  (var found false)
  (each [_ e (ipairs list) &until found]
    (when (pred e)
          (set found true)))
  found)

(fn nums-in-range [[min max] ok?]
  (var t 0)
  (for [i min max 1]
    (when (ok? i)
          (set t (+ t 1))))
  t)

(fn solve-a [range]
  (fn ok? [num]
    (let [ds (digit-deltas num)]
      (and (has? ds #(= $1 0))
           (not (has? ds #(< $1 0))))))

  (nums-in-range range ok?))

(fn solve-b [range]
  (fn ok? [num]
    (let [ds (digit-deltas num)
          dr (digit-runs num)]
      (and (not (has? ds #(< $1 0)))
           (has? dr #(= $1 2)))))

  (nums-in-range range ok?))

{
  : read
  : solve-a
  : solve-b
}
