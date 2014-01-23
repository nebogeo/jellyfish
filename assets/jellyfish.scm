;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jellyfish livecoding stuff

(define (jelly-compiled code)
  (define addr 0)
  (for-each
   (lambda (v)
     (pdata-set! "x" addr v)
     (set! addr (+ addr 1)))
   code))

(define jellyfish '())

;; run 3 at once
(define max-jellyfish 3)

(define (add-jellyfish j l)
  (cond
   ((>= (length l) max-jellyfish)
    (destroy (car l))
    (append (cdr l) (list j)))
   (else
    (append l (list j)))))

(define (make-jelly speed prim-type code)
  (let ((p (build-jellyfish 512)))
    (with-primitive
     p
     (let ((c (compile-program speed prim-type 1 code)))
       ;; (disassemble c)
       (jelly-compiled c))
     (set! jellyfish (add-jellyfish p jellyfish))
     p)))


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
