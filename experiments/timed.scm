(synth-init 20 44100)

(define time (ntp-time-add (ntp-time) 10))

(define (ntp>? a b)
  (or (> (car a) (car b))
      (and (eqv? (car a) (car b))
           (> (cadr a) (cadr b)))))

(define (loop time m)
  (cond ((ntp>? (ntp-time-add (ntp-time) 10) time)
         (msg m)
         (play time (mul (adsr 0 0.1 0 0) (saw (+ 40 (* m 20)))) 0)
         (loop (ntp-time-add time 0.1) (+ m 1)))))


(msg "foooo")

(loop time 0)
