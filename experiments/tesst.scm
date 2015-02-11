(translate (vector -4 0 0))

(with-state
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (hint-none)
 (hint-wire)
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (hint-none)
 (hint-wire)
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (hint-unlit)
 (build-cube))

(identity)
(translate (vector 0 2 0))
(translate (vector -4 0 0))
(texture (load-texture "raspberrypi.png"))

(with-state
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (hint-none)
 (hint-wire)
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (hint-none)
 (hint-wire)
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (build-cube))

(translate (vector 2 0 0))

(with-state
 (colour (vector 1 0 0))
 (hint-unlit)
 (build-cube))
