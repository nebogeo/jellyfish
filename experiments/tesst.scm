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

(translate (vector -4 2 0))

(with-state
 (texture (load-texture "font.png"))
 (build-text "hello"))

(translate (vector 2 -6 0))
(texture 0)

(identity)
(define a (build-cube))
(define b (with-state (parent a) (build-cube)))

;; check intersections
(with-primitive
 a
 (recalc-bb)
 (msg "bb/point-intersect? 1=" (bb/point-intersect? (vector 0 0 0) 0))
 (msg "bb/point-intersect? 0=" (bb/point-intersect? (vector 10 0 0) 0))
 (translate (vector 10 0 0))
 (msg "bb/point-intersect? 1=" (bb/point-intersect? (vector 10 0 0) 0))
 )

;; check bb/point intersect works in global coords
(define c (build-cube))
(with-primitive b (translate (vector 10 0 0)))
(with-primitive c (parent b))

(with-primitive
 c
 (recalc-bb)
 (msg "bb/point-intersect? 1=" (bb/point-intersect? (vector 20 0 0) 0)))
