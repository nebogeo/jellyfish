(synth-init 20 44100)

(play-now (mul
           5
           (echo
           (mul (adsr 0 1 0 0)
                (sine (add 40 (mul (sine 800)
                                   (mul (sine 3) 1000)))))
           0.1 0.9)) 0)
