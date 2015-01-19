(define test-vm (build-jellyfish 4096))

(define tests
  '(let ((a 10))
     (if (> a 0) (trace 1) (trace 0))
     (if (< a 0) (trace 0) (trace 1))
     (write! 1000 (vector 999 999 999))
     (if (eq? (read 1000) (vector 999 999 999)) (trace 1) (trace 0))
     (if (eq? (read 1000) (vector 990 999 999)) (trace 0) (trace 1))

     (define zzz 99)
     (when (eq? zzz 99) (trace 1))
     (when (eq? zzz 88) (trace 0))
     (let ((b 1))
       (trace b))
     (trace b) ;; <-- fixme!


     (cond
      ((eq? 1 1) (trace 1))
      ((eq? 2 2) (trace 0)))
     (cond
      ((eq? 2 1) (trace 0))
      ((eq? 2 2) (trace 1)))
     (cond
      ((eq? 2 1) (trace 0))
      ((eq? 2 3) (trace 0))
      (else (trace 1)))

     (loop (< a 20)
           (when (< a 15)
                 (write-add! 1000 1 2 3 4 5)
                 (trace 99)
                 )
           (cond
            ((eq? 2 1) (trace 0))
            ((eq? 2 3) (trace 0))
            (else (trace 1)))
           (++! a)
           )

     ))

(disassemble-compiled tests)

 (with-primitive
  test-vm
  (program-jelly
   1 prim-triangles
   tests))
