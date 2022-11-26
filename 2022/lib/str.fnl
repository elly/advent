(fn asarray [s]
  (collect [c (s:gmatch ".")] c))

(fn chars [s]
  (s:gmatch "."))

{
  :asarray asarray
  :chars chars
}
