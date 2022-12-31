; 2019/5: Sunny with a Chance of Asteroids

(local ic (require :../lib/intcode))

{
  :read ic.make
  :solve-a #(ic.run-copy-with-io $1 [1])
  :solve-b #(ic.run-copy-with-io $1 [5])
}
