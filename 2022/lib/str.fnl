; lib/str: string library

(fn chars [s]
  (s:gmatch "."))

(fn split [s]
  (icollect [v (s:gmatch "[^%s]+")] v))

(fn tonumz [s]
  (let [r (tonumber s)]
    (if (= r nil)
        0
        r)))

{
  : chars
  : split
  : tonumz
}
