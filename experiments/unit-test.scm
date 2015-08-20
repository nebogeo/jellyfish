(define (load-code fn)
  (let* ((f (open-input-file fn))
         (r (read f)))
    (close-input-port f) r))


(define program (load-code "unit-test.jelly"))

(define jelly-primsize 4096)

(let ((p (build-jellyfish jelly-primsize)))
  (with-primitive
   p
   (program-jelly 1 prim-triangles program)
   (hint-unlit)))
