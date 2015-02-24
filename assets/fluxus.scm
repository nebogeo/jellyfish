;;----------------------------------------------------------------
;; immediate mode (everyone loves immediate mode)

(define immediate-cube (with-state (hint-none) (build-cube)))
(define (draw-cube) (draw-instance immediate-cube))
