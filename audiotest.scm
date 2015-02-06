(display "hello")(newline)

(synth-init)
(display "init done")(newline)

(synth-create 0 2 440.0)
(synth-play 0.0 0 0.0)

(display "done")(newline)
