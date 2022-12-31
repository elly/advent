; 2019/9: Sensor Boost

(local ic (require :../lib/intcode))

{
  :read ic.make
  :solve-a #(ic.run-copy-with-io $1 [1])
  :solve-b #(ic.run-copy-with-io $1 [2])
}
