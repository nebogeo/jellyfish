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
   (build-jellyfish 4096)
   (build-jellyfish 4096)))

(define current 0)

(define (make-jelly speed prim-type code)
  (let ((p (list-ref jellyfish current)))
    (msg p)
    (with-primitive
     p
     (let ((c (compile-program speed prim-type 1 code)))
       ;(disassemble c)
       (jelly-compiled c))
     (set! current (modulo (+ current 1) (length jellyfish)))
     p)))

(define weft
  (make-jelly
   50 prim-triangles
   '(let ((vertex positions-start)
          (t 0)
          (v 0)
          (weft-direction (vector 3 0 0))
          (weft-position (vector 0 0 0)))

      (define right-selvedge
        (lambda (gap)
          ;; top corner
          (write! vertex
                  (- (+ weft-position (vector 3 0 0)) gap)
                  (- (+ weft-position (vector 4 1 0)) gap)
                  (- (+ weft-position (vector 3 1 0)) gap))
          (set! vertex (+ vertex 3))
          ;; vertical connection
          (write! vertex
                  (- (+ weft-position (vector 4 1 0)) gap)
                  (- (+ weft-position (vector 3 1 0)) gap)
                  (+ weft-position (vector 3 0 0))
                  (- (+ weft-position (vector 4 1 0)) gap)
                  (+ weft-position (vector 3 0 0))
                  (+ weft-position (vector 4 0 0)))
          (set! vertex (+ vertex 6))
          ;; bottom corner
          (write! vertex
                  (+ weft-position (vector 3 0 0))
                  (+ weft-position (vector 4 0 0))
                  (+ weft-position (vector 3 1 0)))
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
             (set! weft-position (+ weft-position weft-direction))
             ;; selvedge time?
             (cond
              ((> (mag (*v weft-position (vector 1 0 0))) 40)
               (set! weft-position (- (+ weft-position (vector 0 1.5 0))
                                      weft-direction))
               (set! weft-direction (* weft-direction -1))
               (cond
                ((> 0 weft-direction) (right-selvedge (vector 0 1.5 0)))
                ((< 0 weft-direction) (left-selvedge (vector 0 1.5 0))))))

             (write! vertex
                     weft-position
                     (+ weft-position (vector 3 1 0))
                     (+ weft-position (vector 3 0 0))
                     weft-position
                     (+ weft-position (vector 3 1 0))
                     (+ weft-position (vector 0 1 0)))
             (set! vertex (+ vertex 6)))
       ;;(set! t (+ t 0.01))
       ))))


;;  weave section
;;  top shed
;;  bottom shed
;;  back section

(define warp
  (make-jelly
   3000 prim-triangles
   '(let ((vertex positions-start)
          (warp-end 0)
          (warp-position (vector 0 0 0))
          (shed 0)
          (weft-t 0)
          (draft-pos 0)
          (draft-size 4)
          (draft 1) (d-b 1) (d-c 0) (d-d 0)
          (d-e 0) (d-f 1) (d-g 1) (d-h 0)
          (d-i 0) (d-j 0) (d-k 1) (d-l 1)
          (d-m 1) (d-n 0) (d-o 0) (d-p 1)
          (draft-tmp 0)
          (shed-tmp 0))

      (define build-quad
        (lambda (tl size)
          (write! vertex
                  tl (+ tl size)
                  (+ tl (*v size (vector 1 0 0)))
                  tl (+ tl size)
                  (+ tl (*v size (vector 0 1 0))))
          (set! vertex (+ vertex 6))))

      (define animate-shed
        (lambda (i v)
          (set! shed-tmp (cond ((> v 0.5) (vector 0 0 3))
                               ((< v 0.5) (vector 0 0 -3))))
          (set! v (cond ((< v 0.5) (vector 0 0 3))
                        ((> v 0.5) (vector 0 0 -3))))
          (set! warp-end 0)
          (loop (< warp-end 20)
                (set! draft-tmp
                      (read (+ (addr draft) (+ (* draft-pos draft-size)
                                               (modulo warp-end (+ draft-size (vector 0 1 1)) )))))

                (cond ((> draft-tmp 0.5)
                       (write-add! (- i 6) 0 shed-tmp 0 0 shed-tmp shed-tmp
                                   shed-tmp 0 shed-tmp shed-tmp))
                       ((< draft-tmp 0.5)
                       (write-add! (- i 6) 0 v 0 0 v v
                                   v 0 v v)))

                (set! i (+ i 24))
                (set! warp-end (+ warp-end 1)))))

      (set! vertex positions-start)
      ; build 4 segments X warp-ends
      (loop (< warp-end 20)
            (set! warp-position (+ (vector -25 -35 0)
                                   (* (vector 2.5 0 0) warp-end)))
            (build-quad warp-position (vector 1 35 0))
            (build-quad (+ warp-position (vector 0 35 0)) (vector 1 15 0))
            (build-quad (+ warp-position (vector 0 50 0)) (vector 1 15 0))
            (build-quad (+ warp-position (vector 0 65 0)) (vector 1 25 0))
            (set! warp-end (+ warp-end 1)))

      (forever
       ;; todo control externally

       (set! weft-t (+ weft-t 0.05))
       (cond ((> weft-t 1)
              (trace draft-pos)
              (set! draft-pos (+ draft-pos 1))
              (cond ((> draft-pos draft-size)
                     (set! draft-pos 0)))
              (set! weft-t 0)))

       (set! vertex (+ positions-start 12))
       (animate-shed vertex weft-t)
       (set! shed (+ shed 5))

       ))))


 (define weave-scale (vector 0.1 -0.1 0.1))

(with-primitive
 warp
 (hint-unlit)
 (texture (load-texture "thread.png"))
 (scale weave-scale)
 (pdata-index-map! (lambda (i t)
                     (cond
                      ((eqv? (modulo i 6) 0) (vector 0 0 0))
                      ((eqv? (modulo i 6) 1) (vector 1 10 0))
                      ((eqv? (modulo i 6) 2) (vector 1 0 0))
                      ((eqv? (modulo i 6) 3) (vector 0 0 0))
                      ((eqv? (modulo i 6) 4) (vector 1 10 0))
                      ((eqv? (modulo i 6) 5) (vector 0 10 0))
                      )) "t")
 (pdata-map! (lambda (c) (vector 1 0.5 0.2)) "c")
 (pdata-map! (lambda (n) (vector 0 0 0)) "n"))


(with-primitive
 weft
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
 (pdata-map! (lambda (c) (vector 0.2 0.8 1)) "c")
 (pdata-map! (lambda (n) (vector 0 0 0)) "n"))

(every-frame
 (with-primitive
  weft
  (when
   (< (vy (vtransform  (pdata-ref "x" 11) (get-transform))) 0)
   (translate (vector 0 -0.1 0)))))
