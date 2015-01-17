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
