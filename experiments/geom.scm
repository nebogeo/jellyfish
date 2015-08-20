(define (build-cylinder x y)
  (let ((p (build-polygons (* x y 2) 'triangles)))
    (with-primitive
     p
     (hint-unlit)
     (hint-vertcols)
;;     (hint-wire)
     (pdata-map!
      (lambda (p)
        (rndvec)) "p")
     (pdata-map!
      (lambda (p)
        (vector 1 1 1)) "c"))
    ))

(build-cylinder 10 10)
