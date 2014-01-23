;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jellyfish livecoding stuff

(define (jelly-compiled code)
  (define addr 0)
  (for-each
   (lambda (v)
     (pdata-set! "x" addr v)
     (set! addr (+ addr 1)))
   code))

(define jellyfish
  (list
   (build-jellyfish 512)
   (build-jellyfish 512)
   (build-jellyfish 512)))

(define current 0)

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
         (flingdamp (vector 50 -20 0))
         (world (vector 0 0 0))
         (t 0))

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
                                (read (+ vertex 1)))
                             (- (read vertex)
                                (read (+ vertex 2))))))

           ;; write to normal data
           (write! (+ vertex 512) n n n)

           ;; write the z height as texture coordinates
           (write! (+ vertex 1536)
                   (*v (swizzle zzz a) (vector 0 4 0))
                   (*v (swizzle zzz b) (vector 0 4 0))
                   (*v (swizzle zzz c) (vector 0 4 0))))))

     ;; forever
     (forever

      (define vel (* flingdamp 0.002))
      ;; update the world coordinates
      (set! world (+ world vel))
      (set! t (+ t 0.1))

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
