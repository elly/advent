; 2022/25: Full of Hot Air

(local str (require :../lib/str))
(local tbl (require :../lib/tbl))

(fn unsnafu [s]
  (assert (= (type s) :string))

  (fn pow [base e]
    (var b 1)
    (for [i 1 e 1]
      (set b (* b base)))
    b)

  (fn undigit [d]
    (match d
      :- -1
      := -2
      x (tonumber x)))

  (let [digits (str.explode s)]
    (accumulate [r 0 i d (ipairs (tbl.reverse digits))]
      (+ r (* (undigit d) (pow 5 (- i 1)))))))

(fn snafu [s]
  (assert (= (type s) :number))

  (var r (% s 5))
  (var m (math.floor (/ s 5)))

  (if
    (and (= m 0) (= r 0))        ""
    (or (= r 0) (= r 1) (= r 2)) (.. (snafu m) r)
    (= r 3)                      (.. (snafu (+ m 1)) "=")
    (= r 4)                      (.. (snafu (+ m 1)) "-")))

(fn read [lines]
  (tbl.map lines unsnafu))

(fn solve-a [nums] (snafu (tbl.sum nums)))
(fn solve-b [nums] 0)

(fn check []
  (assert-eq 1 (unsnafu "1"))
  (assert-eq 3 (unsnafu "1="))
  (assert-eq 4 (unsnafu "1-"))
  (assert-eq 5 (unsnafu "10"))
  (assert-eq 20 (unsnafu "1-0"))

  (assert-eq "1=" (snafu 3))
  (assert-eq "122" (snafu 37)))

{
  : check
  : read
  : solve-a
  : solve-b
}
