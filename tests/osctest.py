import osc,time
from random import choice

code = ["""
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
""",

"""
(with-primitive
   (make-jelly 1000 prim-tristrip
    '(let ((vertex positions-start))
       (forever
        (set! vertex positions-start)
        (loop (< vertex positions-end)
              (write! vertex (+ (read vertex) (rndvec)))
              (++! vertex))
       )))
   (pdata-map! (lambda (p) (srndvec)) "p")
   (pdata-map! (lambda (c) (rndvec)) "c"))
""",

"""
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
"""]


while 1:

    print("sending")
    osc.Message("/eval",[choice(code)]).sendto("192.168.1.192",8000)
    #osc.Message("/eval",[choice(code)]).sendlocal(8000)
    time.sleep(60)
