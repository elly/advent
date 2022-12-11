; lib/str: string library

(fn allmatches [s m]
  (icollect [v (s:gmatch m)] v))

(fn allnums [s]
  (icollect [v (s:gmatch "%d+")] (tonumber v)))

(fn chars [s]
  (s:gmatch "."))

(fn explode [s]
  (icollect [v (s:gmatch ".")] v))

(fn split [s]
  (icollect [v (s:gmatch "[^%s]+")] v))

(fn tonumz [s]
  (let [r (tonumber s)]
    (if (= r nil)
        0
        r)))

{
  : allmatches
  : allnums
  : chars
  : explode
  : split
  : tonumz
}
