;  (with-primitive
;   (make-jelly-obj 1000
;    '(let ((vertex positions-start))
;       (forever
;        (set! vertex positions-start)
;        (loop (< vertex positions-end)
;              (write! vertex (+ (read vertex) (rndvec)))
;              (++! vertex))
;       )))
;   (pdata-map! (lambda (p) (srndvec)) "p")
;   (pdata-map! (lambda (c) (rndvec)) "c"))


(with-primitive
 (make-jelly
  10000 prim-tristrip
  '(let ((vertex positions-start)
         (t 0)
         (v 0)
         (np 0))
     (forever

      (set! vertex (+ positions-start 1))
      (loop (< vertex positions-end)
            (set! v (+ (* (normalise (read vertex)) 0.05)
                       (* (- (read vertex)
                             (read (- vertex 1))) -0.5)))
            (write-add! vertex (+ v (* (rndvec) 0.1)))
            (set! vertex (+ vertex 1)))

      (set! t (+ t 0.01))
      )))
 (hint-unlit)
 (pdata-copy "p" "t")
 (pdata-map! (lambda (p) (vmul (srndvec) 1)) "p")
 (pdata-map! (lambda (c) (rndvec)) "c")
 (pdata-map! (lambda (n) (vector 0 0 0)) "n"))

  ;(with-primitive
  ; (make-jelly-obj
  ;  10000 prim-triangles
  ; '(let ((vertex positions-start)
  ;         (t 0)
  ;         (v 0)
  ;         (np 0))
  ;     (forever
  ;      (set! vertex positions-start)
  ;      (loop (< vertex positions-end)
  ;            (set! np (+ (* (+ (read vertex) vertex) 0.1)
  ;                        (swizzle yyx t)))
  ;            (set! v (+ (*v (noise np) (vector 1 0 0))
  ;                       (*v (noise (+ np 101.1)) (vector 0 1 0))))
  ;            (set! v (*v (- v (vector 0.47 0.47 0.47)) (vector 0.1 0.1 0)))
  ;            (write-add! vertex v v v v v v)
  ;            (set! vertex (+ vertex 6)))
  ;      (set! t (+ t 0.01))
  ;      )))
  ; (hint-unlit)
  ; (pdata-index-map!
  ;  (lambda (i p)
  ;    (let ((z (* i 0.01)))
  ;      (if (odd? i)
  ;          (list-ref
  ;           (list (vector 0 0 z) (vector 1 0 z) (vector 1 1 z))
  ;           (modulo i 3))
  ;          (list-ref
  ;           (list (vector 1 1 z) (vector 0 1 z) (vector 0 0 z))
  ;           (modulo i 3))))) "p")
  ; (texture (load-texture "raspberrypi.png"))
  ; (translate (vector -0.5 -0.5 0))
  ; (pdata-copy "p" "t")
  ; (pdata-map! (lambda (t) (vmul t -1)) "t")
  ; (pdata-map! (lambda (c) (vector 1 1 1)) "c")
  ; (pdata-map! (lambda (n) (vector 0 0 0)) "n"))
























(define (terrain-setup)
  (pdata-map! (lambda (n) (vmul (vector (crndf) (crndf) 0) 0.001)) "n")
  (pdata-map! (lambda (c) (vector 1 1 1)) "c")

  (texture (load-texture "stripes.png"))

  (translate (vector -1 2 0))
  (rotate (vector -45 0 0))
;  (rotate (vector 0 0 100))
  (scale (vector 1.5 1.5 1.5))

  (let ((tsize 1)
        (twidth 8))
    (pdata-index-map!
     (lambda (i p)
       (let* ((tpos (modulo i 3))
              (tri (quotient i 3))
              (flip (modulo tri 2))
              (quad (quotient tri 2))
              (col (modulo quad twidth))
              (row (quotient quad twidth)))
         (vadd
          (vector (+ (* row tsize) 10) (* col tsize) 0)
          (vmul
           (if (zero? flip)
               (cond
                ((eqv? tpos 0) (vector 0 0 0))
                ((eqv? tpos 1) (vector tsize 0 0))
                ((eqv? tpos 2) (vector tsize tsize 0)))
               (cond
                ((eqv? tpos 0) (vector 0 0 0))
                ((eqv? tpos 1) (vector tsize tsize 0))
                ((eqv? tpos 2) (vector 0 tsize 0))))
           1))))
     "p"))

  (pdata-map! (lambda (t) (vector 0 0 0)) "t")
  )

(define synthtest
  '(let ((delay 99999))
     (loop 1
           (play-now
            (mul (adsr 0 0.1 0 2)
                 (sine 3000)))

           (loop (> delay 0)
                 (-- delay))
           (set! delay 99999)
           )))

(define (obj-setup)
  (pdata-map! (lambda (p) (srndvec)) "p")
  (pdata-map! (lambda (c) (rndvec)) "c")
  (hint-unlit))


(define obj-test
  '(let ((t 0)
         (v 0)
         (anim 0))

     (define rotx
       (lambda (a)
         (write! reg-tx-rotateb (swizzle zyx (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotatec (swizzle zxy (sincos a)))))

     (define roty
       (lambda (a)
         (write! reg-tx-rotatea (swizzle yzx (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotatec (swizzle xzy (sincos a)))))

     (define rotz
       (lambda (a)
         (write! reg-tx-rotatea (swizzle yxz (*v (sincos a) (vector 1 -1 0))))
         (write! reg-tx-rotateb (swizzle xyz (sincos a)))))


     (define move-along-y
       (lambda (speed)
         (write! reg-tx-translate
                 (+ (read reg-tx-translate)
                    (* (read reg-tx-rotatea) speed)))))

     (define blend
       (lambda (anim)
         (set! v positions-start)
         (loop (< v positions-end)
               (write! v (+ (* (read (+ v 1024)) anim)
                            (* (read (+ v 1536)) (- 1 anim))))
               (set! v (+ v 1)))))

     (loop 1
           (roty (* t 8))
           (move-along-y 0.2)
           (set! anim (* (abs (sincos (* t 30))) 1.5))
           (blend anim)
           (set! t (+ t 1))
           )))

(define terrain
  '(let ((vertex positions-start)
         (flingdamp (vector 5 10 0))
         (world (vector 0 0 0)))

     ;; recycle a triangle which is off the screen
     (define recycle
       (lambda (dir)
         ;; shift along x and y coordinates:
         ;; set z to zero for each vertex
         (write! vertex
                 (+ (*v (read vertex)
                        (vector 1 1 0)) dir)
                 (+ (*v (read (+ vertex 1))
                        (vector 1 1 0)) dir)
                 (+ (*v (read (+ vertex 2))
                        (vector 1 1 0)) dir))

         ;; get the perlin noise values for each vertex
         (let ((a (noise (* (- (read vertex) world) 0.2)))
               (b (noise (* (- (read (+ vertex 1))
                               world) 0.2)))
               (c (noise (* (- (read (+ vertex 2))
                               world) 0.2))))

           ;; set the z coordinate for height
           (write-add!
            vertex
            (+ (*v a (vector 0 0 8))
               (vector 0 0 -4))
            (+ (*v b (vector 0 0 8))
               (vector 0 0 -4))
            (+ (*v c (vector 0 0 8))
               (vector 0 0 -4)))

           ;; recalculate normals
           (define n (normalise
                      (cross (- (read vertex)
                                (read (+ vertex 2)))
                             (- (read vertex)
                                (read (+ vertex 1))))))

           ;; write to normal data
           (write! (+ vertex 512) n n n)

           ;; write the z height as texture coordinates
           (write! (+ vertex 1536)
                   (*v (swizzle zzz a) (vector 0 4 0))
                   (*v (swizzle zzz b) (vector 0 4 0))
                   (*v (swizzle zzz c) (vector 0 4 0))))))

     ;; forever
     (forever
       ;; add inertia to the fling/gamepad joystick input
       (set! flingdamp (+ (* flingdamp 0.99)
                          (*v
                           (read reg-fling)
                           (vector 0.01 -0.01 0))))

       (define vel (* flingdamp 0.002))
       ;; update the world coordinates
       (set! world (+ world vel))

       ;; for each vertex
       (loop (< vertex (- positions-end 3))
         ;; update the vertex position
         (write-add! vertex vel vel vel)

         ;; check for out of area polygons to recycle
         (cond
          ((> (read vertex) 5.0)
           (recycle (vector -10 0 0)))
          ((< (read vertex) -5.0)
           (recycle (vector 10 0 0))))

         (cond
          ((> (swizzle yzz (read vertex)) 4.0)
           (recycle (vector 0 -8 0)))
          ((< (swizzle yzz (read vertex)) -4.0)
           (recycle (vector 0 8 0))))

         (set! vertex (+ vertex 3)))
       (set! vertex positions-start))))


(define jelly 0)
(define jelly2 0)

(define (print-obj c)
  (cond
   ((< c (pdata-size))
    (msg (pdata-ref "p" c))
    (print-obj (+ c 1)))))
