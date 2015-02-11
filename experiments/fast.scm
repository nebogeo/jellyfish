(set! debug #t)
(disassemble-compiled
 '(let ((vertex 0))
    (write! reg-graphics (vector 4096 prim-triangles 2))
    (set! vertex positions-start)
    (loop (< vertex (+ positions-start 200))
          (write! vertex (* (rndvec) 7))
          (set! vertex (+ vertex 1)))
    (forever
     (set! vertex positions-start)
     (loop (< vertex (+ positions-start 200))
           (trace (read reg-control))
           (write! vertex
                   (+ (* (- (read (- vertex 1)) (read vertex)) 0.01)
                      (+ (* (read vertex) 0.999)
                         (* (rndvec) 0.01))))
           (set! vertex (+ vertex 1))))))

(set! debug #f)

(with-primitive
 (build-jellyfish 4096)
 (pdata-map! (lambda (c) (vector 1 1 1)) "c")
 (hint-unlit)
 (program-jelly
  3000
  prim-triangles
  '(let ((vertex 0))
     (write! reg-graphics (vector 4096 prim-triangles 2))
     (set! vertex positions-start)
     (loop (< vertex (+ positions-start 200))
           (write! vertex (* (rndvec) 7))
           (set! vertex (+ vertex 1)))
     (forever
      (set! vertex positions-start)
      (loop (< vertex (+ positions-start 200))
            (write! vertex
                    (+ (* (- (read (- vertex 1)) (read vertex)) 0.01)
                       (+ (* (read vertex) 0.999)
                          (* (rndvec) 0.01))))
            (set! vertex (+ vertex 1)))))))
