(define (spikey n)
  (when (not (zero? n))
        (with-state
         (rotate (vmul (vmul (vector n n n) 0.01) 360))
         (scale (vector 0.1 10 (* 5 (sin (* (+ (time) n) 0.1)))))
         (draw-cube))
        (spikey (- n 1))))

(every-frame
 (colour (vector 1 0.8 0.5))
 (rotate (vector (sin (* 0.01 (time)))
                 (cos (* 0.02 (time))) 0))
 (spikey 40))
