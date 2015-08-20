(synth-init "fluxa" 44100 2048 10)

(define (rr l u)
  (+ l (* (rndf) (- u l))))

(define (ntp>? a b)
  (or (> (car a) (car b))
      (and (eqv? (car a) (car b))
           (> (cadr a) (cadr b)))))


(define deadline (ntp-time-add (ntp-time) 0.5))

(define (rnd-adsr)
    (adsr (rr 0 0.5)
          (rr 0 0.5)
          (rr 0 1)
          (rr 0 0.5)))

(define (fm-unit freq)
  (mul
   (sine (mul freq (rr 10 10000)))
   (rnd-adsr)
   ))


(define (fm freq)
  (fm-carrier (fm-signal freq)))

(define (fm-one)
  (add
   (fm-unit (rndf))
   (fm-unit (fm-unit (rndf))))
;   (add
;    (fm-unit (fm-unit (rndf)))
;    (fm-unit (rndf)))
   )

(define (fm-five)
  (add
   (fm-unit
    (add
     (add
      (fm-unit (rndf))
      (fm-unit (rndf)))
     (fm-unit (rndf))))
   (fm-unit (rndf))))

(define (fm-six)
  (add
   (add
    (fm-unit (rndf))
    (fm-unit (rndf)))
   (fm-unit (fm-unit (fm-unit (rndf))))))

(define (fm-eleven)
  (add
   (add
    (fm-unit (rndf))
    (fm-unit (rndf)))
   (fm-unit (add
             (fm-unit (rndf))
             (fm-unit (fm-unit (rndf)))))))

(define (fm-twenty)
  (add
   (fm-unit (fm-unit (fm-unit (rndf))))
   (fm-unit (fm-unit (fm-unit (rndf))))))

(define (fm-twentytwo)
  (add
   (fm-unit (fm-unit (fm-unit (fm-unit (rndf)))))
   (fm-unit (fm-unit (rndf)))))

(define (fm-twentythree)
  (add
   (add
    (fm-unit (rndf))
    (add
     (fm-unit (rndf))
     (fm-unit (rndf))))
   (add
    (fm-unit (rndf))
    (add
     (fm-unit (rndf))
     (fm-unit (rndf))))))

(define (rndfm)
  (let ((d 0))
    (cond
     ((eqv? d 0) (fm-one))
     ((eqv? d 1) (fm-five))
     ((eqv? d 2) (fm-six))
     ((eqv? d 3) (fm-eleven))
     ((eqv? d 4) (fm-twenty))
     ((eqv? d 5) (fm-twentytwo))
     ((eqv? d 6) (fm-twentythree)))))


(synth-record "randomiz")

(every-frame
 (when (ntp>? (ntp-time) deadline)
       (play-now
        (mul
         (adsr 0 0 1 1)
         (rndfm))
         0)
       (set! deadline (ntp-time-add deadline 1))))
