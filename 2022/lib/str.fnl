; lib/str: string library

(fn chars [s]
  (s:gmatch "."))

(fn tonumz [s]
  (let [r (tonumber s)]
    (if (= r nil)
        0
        r)))

{
  :chars chars
  :tonumz tonumz
}
