; 2019/8: Space Image Format

(local cloud (require :../lib/cloud))
(local list (require :../lib/list))

(fn read [lines]
  (fn read-dim [line]
    (let [parts (icollect [v (line:gmatch "%d+")] (tonumber v))]
      (values (. parts 1) (. parts 2))))

  (fn slice [line nchars]
    (values (line:sub 1 nchars) (line:sub (+ nchars 1))))

  (fn char->px [c]
    (if (= c "0") :black
        (= c "1") :white
        (= c "2") :trans))

  (fn str->pxes [s]
    (icollect [v (s:gmatch "%d")] (char->px v)))

  (fn read-plane! [image plane rows cols z]
    (for [y 1 rows 1]
      (for [x 1 cols 1]
        (cloud.add! image [x y z] (. plane (+ (* (- y 1) cols) x))))))

  (fn read-image [line rows cols]
    (var line line)
    (var image (cloud.make))
    (var z 0)
    (while (> (# line) 0)
      (let [(plane rest) (slice line (* rows cols))]
        (read-plane! image (str->pxes plane) rows cols z)
        (set line rest)
        (set z (+ z 1))))
    image)

  (let [(rows cols) (read-dim (. lines 1))]
    (read-image (. lines 2) rows cols)))

(fn planes [image]
  (fcollect [i image.bounds.z.min image.bounds.z.max 1]
    (icollect [_ p (ipairs (cloud.all image #(= (. $1 3) i)))]
      (cloud.get image p))))

(fn count [list val]
  (# (icollect [_ v (ipairs list)] (if (= v val) v nil))))

(fn solve-a [image]
  (let [(p _) (list.maximize (planes image) #(* -1 (count $1 0)))]
    (* (count p 1) (count p 2))))

(fn flatten [image]
  (var r (cloud.make))
  (for [y image.bounds.y.min image.bounds.y.max 1]
    (for [x image.bounds.x.min image.bounds.x.max 1]
      (var got nil)
      (for [z image.bounds.z.min image.bounds.z.max 1 &until got]
        (let [v (cloud.get image [x y z])]
          (when (not (= v :trans))
            (set got v))))
      (cloud.add! r [x y 0] got)))
  r)

(fn raster [image]
  (for [y image.bounds.y.min image.bounds.y.max 1]
    (for [x image.bounds.x.min image.bounds.x.max 1]
      (let [p (cloud.get image [x y 0])]
        (io.write (if (= p :white) "#" "."))))
    (io.write "\n")))

(fn solve-b [image]
  (let [r (flatten image)]
    (raster r)
    0))

{
  : read
  : solve-a
  : solve-b
}
