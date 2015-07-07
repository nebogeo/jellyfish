(synth-init "fluxa" 44100 2048 20)

(play-now
 (mul
  (adsr 0 10 0 0)
  (pad 200 2 0.4 0.1)
  )
  0)
