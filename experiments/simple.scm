(msg "started")

(clear)
(clear-colour (vector 1 0 0 0.5))
(build-cube)

(define (spikey n)
  (when (not (zero? n))
        (with-state
         (rotate (vmul (vector n n n) 360))
         (scale (vector 0.1 10 0.1))
         (draw-cube))
        (spikey (- n 1))))

(every-frame
 (rotate (vector (* 10 (sin (* 0.01 (time))))
                 (* 10 (cos (* 0.02 (time)))) 0))
 (spikey 40))
