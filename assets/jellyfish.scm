;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define player 
  (list
   (vector 0 6 0)
   0
   (with-state
    (translate (vector 0 6 0))
    (build-cube))))

(define (player-pos player) (list-ref player 0))
(define (player-modify-pos player v) (list-replace player 0 v))
(define (player-dir player) (list-ref player 1))
(define (player-modify-dir player v) (list-replace player 1 v))
(define (player-root player) (list-ref player 2))

(define (rot2d v a)
  (vector (+ (* (vx v) (sin a))
	     (* (vz v) (cos a)))
	  (vy v)
	  (+ (* (vx v) (cos a))
	     (* (vz v) (- (sin a))))))

(define (update-player player)
  (with-primitive 
   (player-root player)
   (identity)
   (translate (player-pos player))
   (rotate (vector 0 (player-dir player) 0)))

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
     (rot2d (vector 0 0 1) (- (player-dir player)))
     (cond
      ((key-pressed "w") 0.2)
      ((key-pressed "s") -0.2)
      (else 0))))))
      
   
(define level 
  (with-state
   (texture (load-texture "bg.png"))
   (scale (vector 5 5 5))
   (load-obj "assets/testlevel.obj")))

(with-primitive 
 level
 (pdata-map! 
  (lambda (t) (vmul t 5)) "t"))

(lock-camera (player-root player))

(every-frame 
 (set! player (update-player player))
 (with-primitive 
  (player-root player)
  (rotate (vector 0 (if (key-pressed "a") 0.5 0) 0))))


<<<<<<< HEAD
=======
(define (make-jelly speed prim-type code)
  (let ((p (list-ref jellyfish current)))
    (msg p)
    (with-primitive
     p
     (let ((c (compile-program speed prim-type 1 code)))
       ;; (disassemble c)
       (jelly-compiled c))
     (set! current (modulo (+ current 1) (length jellyfish)))
     p)))


(with-primitive
  (make-jelly
   10000 prim-triangles
   '(let ((vertex positions-start)
           (t 0)
           (v 0)
           (np 0))
       (forever
        (set! vertex positions-start)
        (loop (< vertex positions-end)
              (set! np (+ (* (+ (read vertex) vertex) 0.1)
                          (swizzle yyx t)))
              (set! v (+ (*v (noise np) (vector 1 0 0))
                        (*v (noise (+ np 101.1)) (vector 0 1 0))))
             (set! v (*v (- v (vector 0.47 0.47 0.47)) (vector 0.1 0.1 0)))
              (write-add! vertex v v v v v v)
              (set! vertex (+ vertex 6)))
        (set! t (+ t 0.01))
        )))
   (hint-unlit)
   (pdata-index-map!
    (lambda (i p)
      (let ((z (* i 0.01)))
        (if (odd? i)
            (list-ref
             (list (vector 0 0 z) (vector 1 0 z) (vector 1 1 z))
             (modulo i 3))
            (list-ref
             (list (vector 1 1 z) (vector 0 1 z) (vector 0 0 z))
             (modulo i 3))))) "p")
   (texture (load-texture "raspberrypi.png"))
   (translate (vector -0.5 -0.5 0))
   (pdata-copy "p" "t")
   (pdata-map! (lambda (t) (vmul t -1)) "t")
   (pdata-map! (lambda (c) (vector 1 1 1)) "c")
   (pdata-map! (lambda (n) (vector 0 0 0)) "n"))
>>>>>>> c0810fc558c7dd18b7fd92214be0f86de69524b4
