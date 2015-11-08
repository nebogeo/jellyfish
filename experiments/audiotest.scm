(synth-init "fluxa" 44100 2048 20)

(play-now
 (mul
  (echo
  (mul (adsr 0 1 0 0)
       (sine (add 40 (mul (sine 800)
                          (mul (sine 3) 1000)))))
  0.1 0.9)
  0.05)
 0)

(play-now (sine (add 100 (mul (add (sine 1.3) (sine 1)) 100))) 0)
