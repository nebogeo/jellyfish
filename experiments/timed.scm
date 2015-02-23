(synth-init)

(define (time-add t secs)
  (list (+ (car t) secs) (cadr t)))

(define time (time-add (time-of-day) 20))

(define (loop time m)
  (when (< m 100)
	(display time)(newline)
	(play time (mul (adsr 0 0.1 0.3 1) (saw (+ 40 (* m 20)))) 0)
	(loop (time-add time 1) (+ m 1))))


(loop time 0)
  