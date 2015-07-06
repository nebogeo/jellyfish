(define player
  (list
   (vector 0 6 0)
   45
   (with-state
    (translate (vector 0 6 0))
    (build-cube))))

(define (player-pos player) (list-ref player 0))
(define (player-modify-pos player v) (list-replace player 0 v))
(define (player-dir player) (list-ref player 1))
(define (player-modify-dir player v) (list-replace player 1 v))
(define (player-root player) (list-ref player 2))

(define (rot2d v a)
  (vector (+ (* (vx v) (cos a))
             (* (vz v) (sin a)))
          (vy v)
          (+ (* (vx v) (- (sin a)))
             (* (vz v) (cos a)))))

(define (update-player player)
  (with-primitive
   (player-root player)
   (identity)
   (translate (player-pos player))
   (rotate (vector 0 (- (player-dir player)) 0)))

  (player-modify-pos
   (player-modify-dir
    player
    (+ (player-dir player)
       (cond
	((key-pressed "a") -1)
	((key-pressed "d") 1)
	(else 0))))
   (vadd
    (player-pos player)
    (vmul
     (rot2d (vector 1 0 0) (player-dir player))
     (cond
      ((key-pressed "w") 0.2)
      ((key-pressed "s") -0.2)
      (else 0))))))


(define level
  (with-state
   (texture (load-texture "bg.png"))
   (scale (vector 5 5 5))
   (load-obj "../assets/testlevel.obj")))

(with-primitive
 level
 (pdata-map!
  (lambda (t) (vmul t 15)) "t"))

(lock-camera (player-root player))

(every-frame
 (set! player (update-player player)))
; (with-primitive
;  (player-root player)
;  (rotate (vector 0 (if (key-pressed "a") 0.5 0) 0))))
