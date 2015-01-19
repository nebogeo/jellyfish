;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weavecoding raspberry pi installation

(rotate (vector 0 -45 0))
(define weft (build-jellyfish 4096))
(define warp (build-jellyfish 4096))
(define weave-scale (vector 0.2 -0.2 0.2))

(with-primitive
 weft
 (program-jelly
  30 prim-triangles
  '(let ((vertex positions-start)
         (t 0)
         (v 0)
         (weft-direction (vector 2 0 0))
         (weft-position (vector -20 0 0))
         (weft-t 0)
         (draft-pos 0)
         (draft-size 4)
         (draft 1) (d-b 0) (d-c 0) (d-d 1)
         (d-e 1) (d-f 1) (d-g 0) (d-h 0)
         (d-i 0) (d-j 1) (d-k 1) (d-l 0)
         (d-m 0) (d-n 0) (d-o 1) (d-p 1)
         (weft-z (vector 0 0 0))
         (weft-count 0)
         (weft-total 21))

     (define read-draft
       (lambda ()
         (read
          (+ (addr draft)
             (+ (* draft-pos draft-size)
                (if (> weft-direction 0)
                    (modulo weft-count (+ draft-size (vector 0 1 1)) )
                    (modulo (- (- weft-total 1) weft-count) (+ draft-size (vector 0 1 1)) )))))))

     (define calc-weft-z
       (lambda ()
         (set! weft-count (+ weft-count 1))
         (if (> (read-draft) 0.5)
             (set! weft-z (vector 0 0 0.01))
             (set! weft-z (vector 0 0 -0.01)))
         ))


     (define right-selvedge
       (lambda (gap)
         ;; top corner
         (write! vertex
                 (- (+ weft-position (vector 2 0 0)) gap)
                 (- (+ weft-position (vector 3 1 0)) gap)
                 (- (+ weft-position (vector 2 1 0)) gap))
         (set! vertex (+ vertex 3))
         ;; vertical connection
         (write! vertex
                 (- (+ weft-position (vector 3 1 0)) gap)
                 (- (+ weft-position (vector 2 1 0)) gap)
                 (+ weft-position (vector 2 0 0))
                 (- (+ weft-position (vector 3 1 0)) gap)
                 (+ weft-position (vector 2 0 0))
                 (+ weft-position (vector 3 0 0)))
         (set! vertex (+ vertex 6))
         ;; bottom corner
         (write! vertex
                 (+ weft-position (vector 2 0 0))
                 (+ weft-position (vector 3 0 0))
                 (+ weft-position (vector 2 1 0)))
         (set! vertex (+ vertex 3))
         ))

     (define left-selvedge
       (lambda (gap)
         ;; top corner
         (write! vertex
                 (- (+ weft-position (vector 0 0 0)) gap)
                 (- (+ weft-position (vector -1 1 0)) gap)
                 (- (+ weft-position (vector 0 1 0)) gap))
         (set! vertex (+ vertex 3))
         ;; vertical connection
         (write! vertex
                 (- (+ weft-position (vector -1 1 0)) gap)
                 (- (+ weft-position (vector 0 1 0)) gap)
                 (+ weft-position (vector 0 0 0))
                 (- (+ weft-position (vector -1 1 0)) gap)
                 (+ weft-position (vector 0 0 0))
                 (+ weft-position (vector -1 0 0)))
         (set! vertex (+ vertex 6))
         ;; bottom corner
         (write! vertex
                 (+ weft-position (vector 0 0 0))
                 (+ weft-position (vector -1 0 0))
                 (+ weft-position (vector 0 1 0)))
         (set! vertex (+ vertex 3))
         ))

     (forever
      (set! vertex positions-start)
      (loop (< vertex positions-end)
            (calc-weft-z)
            (set! weft-position (+ weft-position weft-direction))
            ;; selvedge time?
            (when (> weft-count weft-total)
                  (set! weft-count 0)
                  (set! draft-pos (+ draft-pos 1))
                  (when (> draft-pos draft-size)
                        (set! draft-pos 0))
                  (set! weft-position (- (+ weft-position (vector 0 1.5 0))
                                         weft-direction))
                  (set! weft-direction (* weft-direction -1))
                  (if (> 0 weft-direction)
                      (right-selvedge (vector 0 1.5 0))
                      (left-selvedge (vector 0 1.5 0))))

            (set! weft-t (/ weft-count 21))

            (write! vertex
                    (+ weft-z weft-position)
                    (+ weft-position (+ weft-z (vector 2 1 0)))
                    (+ weft-position (+ weft-z (vector 2 0 0)))
                    (+ weft-z weft-position)
                    (+ weft-position (+ weft-z (vector 2 1 0)))
                    (+ weft-position (+ weft-z (vector 0 1 0))))
            (set! vertex (+ vertex 6)))
      ;;(set! t (+ t 0.01))
      )))
 (hint-unlit)
 (texture (load-texture "thread.png"))
 (scale weave-scale)
 (pdata-index-map! (lambda (i t)
                     (cond
                      ((eqv? (modulo i 6) 0) (vector 0 0 0))
                      ((eqv? (modulo i 6) 1) (vector 1 1 0))
                      ((eqv? (modulo i 6) 2) (vector 1 0 0))
                      ((eqv? (modulo i 6) 3) (vector 0 0 0))
                      ((eqv? (modulo i 6) 4) (vector 1 1 0))
                      ((eqv? (modulo i 6) 5) (vector 0 1 0))
                      )) "t")
 (pdata-map! (lambda (c) (vector 0 0 1)) "c")
 (pdata-map! (lambda (n) (vector 0 0 0)) "n"))


;;  weave section
;;  top shed
;;  bottom shed
;;  back section

(with-primitive
 warp
  (program-jelly
   800 prim-triangles
   '(let ((vertex positions-start)
          (warp-end 0)
          (warp-position (vector 0 0 0))
          (v 0)
          (weft-t 0)
          (draft-pos 0)
          (draft-size 4)
         (draft 1) (d-b 0) (d-c 0) (d-d 1)
         (d-e 1) (d-f 1) (d-g 0) (d-h 0)
         (d-i 0) (d-j 1) (d-k 1) (d-l 0)
         (d-m 0) (d-n 0) (d-o 1) (d-p 1)
         (last-t 0))

      (define build-quad
        (lambda (tl size)
          (write! vertex
                  tl (+ tl size)
                  (+ tl (*v size (vector 1 0 0)))
                  tl (+ tl size)
                  (+ tl (*v size (vector 0 1 0))))
          (set! vertex (+ vertex 6))))

      ;; like weft but don't need to take account of direction
      (define read-draft
        (lambda ()
          (read (+ (addr draft)
                   (+ (* draft-pos draft-size)
                      (modulo warp-end (+ draft-size (vector 0 1 1)) ))))))

      (define animate-shed
        (lambda (i)
          (set! v (if (< weft-t 0.2)
                      (vector 0 0 2)
                      (if (> weft-t 0.8)
                          (vector 0 0 -1.3)
                          (vector 0 0 0))))
          (set! warp-end 0)
          (loop (< warp-end 20)
                (when (< (read-draft) 0.5)
                      (write-add! (- i 6) 0 v 0 0 v v
                                  v 0 v v))
                (set! i (+ i 24))
                (set! warp-end (+ warp-end 1)))))

      (define build-warp
        (lambda ()
          (set! vertex positions-start)
          ;; build 4 segments X warp-ends
          (set! warp-end 0)
          (loop (< warp-end 20)
                (set! warp-position (+ (vector -19 -35.5 0)
                                       (* (vector 2 0 0) warp-end)))
                (build-quad warp-position (vector 1 35 0))
                (build-quad (+ warp-position (vector 0 35 0)) (vector 1 10 0))
                (build-quad (+ warp-position (vector 0 45 0)) (vector 1 15 0))
                (build-quad (+ warp-position (vector 0 60 0)) (vector 1 25 0))
                (set! warp-end (+ warp-end 1)))))

      (build-warp)
      (forever
       (set! vertex (+ positions-start 12))
       (animate-shed vertex)

       (when (> (- last-t weft-t) 0.1)
             (set! draft-pos (+ draft-pos 1))
             (when (> draft-pos draft-size) (set! draft-pos 0))
             (build-warp))

       (set! last-t weft-t)
       )))

 (hint-unlit)
 (texture (load-texture "thread.png"))
 (scale weave-scale)
 (pdata-index-map! (lambda (i t)
                     (cond
                      ((eqv? (modulo i 6) 0) (vector 0 0 0))
                      ((eqv? (modulo i 6) 1) (vector 10 1 0))
                      ((eqv? (modulo i 6) 2) (vector 0 1 0))
                      ((eqv? (modulo i 6) 3) (vector 0 0 0))
                      ((eqv? (modulo i 6) 4) (vector 10 1 0))
                      ((eqv? (modulo i 6) 5) (vector 10 0 0))
                      )) "t")
 (pdata-map! (lambda (c) (vector 1 0.5 0.2)) "c")
 (pdata-map! (lambda (n) (vector 0 0 0)) "n")
  )




(every-frame
 (with-primitive
  warp
  (pdata-set!
   "x" 11 (with-primitive
           weft
           (pdata-ref "x" 12))))
 ;; (with-primitive
 ;;  warp
 ;;  (pdata-set!
 ;;   "x" 12 (with-primitive
 ;;           weft
 ;;           (pdata-ref "x" 13))))

 (with-primitive
  weft
  (when
   (< (vy (vtransform  (pdata-ref "x" 11) (get-transform))) 0)
   (translate (vector 0 -0.1 0)))))
