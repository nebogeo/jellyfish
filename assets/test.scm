(define test-vm (build-jellyfish 4096))

(define tests
  '(let ((a 10))
     (if (> a 0) (trace 1) (trace 0))
     (if (< a 0) (trace 0) (trace 1))
     (trace (read reg-control))
     (write! 1000 (vector 999 999 999))
     (trace (read reg-control))
     ))

(disassemble-compiled tests)

 (with-primitive
  test-vm
  (program-jelly
   1 prim-triangles
   tests))
