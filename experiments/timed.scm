(synth-init 20 44100)

(define time (ntp-time-add (ntp-time) 2))

(define (loop time m)
  (when (< m 20)
        (play time (mul (adsr 0 0.1 0.3 1) (saw (+ 400 (* m 20)))) 0)
        (loop (ntp-time-add time 0.1) (+ m 1))))


(loop time 0)
