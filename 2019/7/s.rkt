#lang racket

(provide today)
(require "../intcode.rkt")

; For part B, we need to be able to stop and start the same VM multiple times
; to feed it bits of IO as it works. I think we can do this with clever use of
; inproc and outproc...
(define (boot-amp-vm vm mode)
  (let ((vm (icvm-copy vm)))
    (set-icvm-inproc! vm
      (lambda ()
        (begin
          (set-icvm-halted! vm 'io)
          mode)))
    (icvm-run! vm)
    vm))

(define (run-amp-vm! vm in)
  (define r #f)
  (set-icvm-halted! vm #f)
  (set-icvm-inproc! vm (const in))
  (set-icvm-outproc! vm
    (lambda (o)
      (set! r o)
      (set-icvm-halted! vm 'io)))
  (icvm-run! vm)

  ; This is highly dubious: 'in' is definitely not the right output value, but
  ; returning it here significantly simplifies the logic in the fold below in
  ; run-amp, so here it is.
  (or r in))

(define (all-halted? vms)
  (andmap (compose (curry equal? 'halt) icvm-halted) vms))

(define (run-amp vm modes)
  (let ((vms (map (curry boot-amp-vm vm) modes)))
    (let loop ((v 0))
      (if (all-halted? vms)
          v
          (loop (foldl run-amp-vm! v vms))))))

(define (optimize vm modes)
  (apply max (map (curry run-amp vm)
                  (permutations modes))))

(define today (list (curryr icvm-load 8192)
                    identity
                    (curryr optimize '(0 1 2 3 4))
                    (curryr optimize '(5 6 7 8 9))))
