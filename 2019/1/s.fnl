; 2019/1: The Tyranny of the Rocket Equation

(fn fuel-needed-simple [mass]
  (- (math.floor (/ mass 3)) 2))

(fn fuel-needed-rec [mass]
  (let [f (fuel-needed-simple mass)]
    (if (> f 0)
        (+ f (fuel-needed-rec f))
        0)))

(fn read [lines]
  (icollect [_ line (ipairs lines)]
    (tonumber line)))

(fn check []
  (assert (= 2 (fuel-needed-simple 12)))
  (assert (= 2 (fuel-needed-simple 14)))
  (assert (= 33583 (fuel-needed-simple 100756)))

  (assert (= 2 (fuel-needed-rec 14)))
  (assert (= 966 (fuel-needed-rec 1969)))
  (assert (= 50346 (fuel-needed-rec 100756))))

(fn solve-a [spec]
  (accumulate [s 0 _ mass (ipairs spec)]
    (+ s (fuel-needed-simple mass))))

(fn solve-b [spec]
  (accumulate [s 0 _ mass (ipairs spec)]
    (+ s (fuel-needed-rec mass))))

{
  : check
  : read
  : solve-a
  : solve-b
}
