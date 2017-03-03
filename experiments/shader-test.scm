(clear)

(define p (build-cube))


(with-primitive
 p
 (shader (slurp "assets/shaders/gooch.vert.glsl")
         (slurp "assets/shaders/gooch.frag.glsl"))
 (shader-set! "LightPos" (vector 0 0 0))
 (shader-set! "WarmColour" (vector 1 0.7 0.4))
 (shader-set! "CoolColour" (vector 0.4 0.7 1))
 (shader-set! "SurfaceColour" (vector 0.3 0.3 1))
 (shader-set! "OutlineWidth" 0.3)
 )

(every-frame
 (with-primitive
  p
  (rotate (vector 1 1 0))))
