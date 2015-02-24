; lz/nz
(synth-init 10 44100)

(define (make-lz md d stk w h mem)
  (vector md d stk w h mem))

(define (lz-md l) (vector-ref l 0))
(define (lz-d l) (vector-ref l 1))
(define (lz-stk l) (vector-ref l 2))
(define (lz-w l) (vector-ref l 3))
(define (lz-h l) (vector-ref l 4))
(define (lz-mem l) (vector-ref l 5))

(define (set-lz-d! l v) (vector-set! l 1 v))
(define (set-lz-stk! l v) (vector-set! l 2 v))
(define (set-lz-mem! l v) (vector-set! l 5 v))

(define (build-lz md w h)
  (make-lz md 0 '((0 0)) w h (make-vector (* w h) #\ )))

(define (lz-poke lz x y s)
  (vector-set! (lz-mem lz) (+ x (* (lz-w lz) y)) s))

(define (lz-peek lz x y)
  (vector-ref (lz-mem lz) (+ x (* (lz-w lz) y))))

(define (lz-read lz)
  (let ((top (lz-top lz)))
    (lz-peek lz (car top) (cadr top))))

(define (lz-pop! lz)
  (when (> (lz-d lz) 0)
	(set-lz-d! lz (- (lz-d lz) 1)))
  (let ((tmp (car (lz-stk lz))))
    (when (not (eqv? (length (lz-stk lz)) 1))
	  (set-lz-stk! lz (cdr (lz-stk lz))))
    tmp))

(define (lz-push! lz item)
  (when (< (lz-d lz) (lz-md lz))
    (set-lz-d! lz (+ (lz-d lz) 1))
    (set-lz-stk! lz (cons item (lz-stk lz)))))

(define (lz-top lz)
  (car (lz-stk lz)))

(define (set-lz-top! lz s)
  (set-lz-stk! lz (cons s (cdr (lz-stk lz)))))

(define (lz-inc-pos lz)
  (set-lz-top! lz (list (+ (car (lz-top lz)) 1) (cadr (lz-top lz)))))

(define (lz-tick lz)
  (lz-inc-pos lz)
  (when (>= (car (lz-top lz)) 8)
       (cond
         ((eqv? (length (lz-stk lz)) 1)
          (set-lz-top! lz (list 0 0)))
         (else
           (lz-pop! lz))))

  (let ((pos (car (lz-top lz)))
        (pat (cadr (lz-top lz)))
        (data (lz-read lz)))
   ; (printf "~a ~a ~a ~a~n" pos pat data (lz-stk lz))
    (cond
      ((char=? data #\ )
       (cond
         ((eqv? (length (lz-stk lz)) 1)
          (set-lz-top! lz (list 0 0)))
         (else
           (lz-pop! lz))))
      ((char=? data #\A) (lz-inc-pos lz) (lz-push! lz (list 0 0)))
      ((char=? data #\B) (lz-inc-pos lz) (lz-push! lz (list 0 1)))
      ((char=? data #\C) (lz-inc-pos lz) (lz-push! lz (list 0 2))))
    (lz-read lz)))

(define (lz-prog lz pat str)
  (let ((c 0))
    (for-each
     (lambda (item)
       (lz-poke lz c pat item)
       (set! c (+ c 1)))
     (string->list str))))

; nz

(define max-vals 16)

(define (make-nz lz vals vx sz cur-t tk off)
  (vector lz vals vx sz cur-t tk off))

(define (nz-lz n) (vector-ref n 0))
(define (nz-vals n) (vector-ref n 1))
(define (nz-vx n) (vector-ref n 2))
(define (nz-sz n) (vector-ref n 3))
(define (nz-cur-t n) (vector-ref n 4))
(define (nz-tk n) (vector-ref n 5))
(define (nz-off n) (vector-ref n 6))

(define (set-nz-vals! n v) (vector-set! n 1 v))
(define (set-nz-vx! n v) (vector-set! n 2 v))
(define (set-nz-cur-t! n v) (vector-set! n 4 v))

(define (t) (ntp-time))

(define (build-nz lz sz tk)
  (make-nz lz '(60) 0 sz (ntp-time-add (t) 5) tk 1.0))

(define (nz-pop! nz)
  (let ((tmp (car (nz-vals nz))))
    (when (not (eqv? (length (nz-vals nz)) 1))
      (set-nz-vals! nz (cdr (nz-vals nz))))
    tmp))

(define (nz-push! nz item)
  (when (< (length (nz-vals nz)) max-vals)
    (set-nz-vals! nz (cons item (nz-vals nz)))))

(define (nz-dup! nz)
  (nz-push! nz (car (nz-vals nz))))

(define (ntp>? a b)
  (or (> (car a) (car b))
      (and (eqv? (car a) (car b))
           (> (cadr a) (cadr b)))))

(define (nz-tick nz)
  (when (ntp>? (ntp-time-add (t) (nz-off nz)) (nz-cur-t nz))
        (set-nz-cur-t! nz (ntp-time-add (nz-cur-t nz) (nz-tk nz)))
        (let ((t (lz-tick (nz-lz nz)))
              (v (car (nz-vals nz))))
          (cond
           ((char=? t #\+) (set-nz-vals! nz (cons (+ (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
           ((char=? t #\-) (set-nz-vals! nz (cons (- (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
           ((char=? t #\<) (set-nz-vx! nz (modulo (- (nz-vx nz) 1) (length (nz-sz nz)))))
           ((char=? t #\>) (set-nz-vx! nz (modulo (+ (nz-vx nz) 1) (length (nz-sz nz)))))
           ((char=? t #\a) (play (nz-cur-t nz) ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 0) v) 0))
           ((char=? t #\b) (play (nz-cur-t nz) ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 1) v) 0))
           ((char=? t #\c) (play (nz-cur-t nz) ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 2) v) 0))
           ((char=? t #\d) (play (nz-cur-t nz) ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 3) v) 0))
           ((char=? t #\[) (nz-dup! nz))
           ((char=? t #\]) (nz-pop! nz)))
          )))

; --

(define l (build-lz 9 8 3))

;(lz-prog l 0 "cCBca-aa")
;(lz-prog l 1 "c-d-c<.d")
;(lz-prog l 2 "b++b+ACd")

;(lz-prog l 0 "aB")
;(lz-prog l 1 "-d>-AC-A")
;(lz-prog l 2 "b+b--bAB")

(lz-prog l 0 " B C")
(lz-prog l 1 "abCcb")
(lz-prog l 2 "ccBdb")



(define ss
  (list
   (list
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (sine (add (mul 20 (sine 4)) (note v)))))
    (lambda (v) (mul (adsr 0 0.1 0 0) (mul 0.2 (add (saw (* 1.5 (note v)))
						    (saw (note v))))))
    (lambda (v) (mul (adsr 0 0.1 0 0)
		     (moogbp (squ (add 10 (mul 1000 (pow (adsr 0 0.2 0 0) 10))))
			     (* v 0.1) 0.1)))
    (lambda (v) (mul (adsr 0 0.02 0 0) (moogbp (white 4) (* v 0.01) 0.45))))
   (list
    (lambda (v) (mul (adsr 0 0.03 0.1 1) (mooghp (saw (* (note v) 0.5))
						 (mul 0.2 (adsr 0.5 0 0 0)) 0.45)))
    (lambda (v) (mul (adsr 0 0.1 0.1 1) (mooglp (add (saw (* 1.5 (note v))) (saw (note v)))
						(* v 0.12) 0.4)))
    (lambda (v) (mul (adsr 0 0.1 0 0) (sine (add
					     (fmod (* v 50) 300)
					     (mul 1000 (pow (adsr 0 0.2 0 0) 10))))))
    (lambda (v) (mul (adsr 0.04 0.02 0 0) (mooglp (white 4) (* v 0.01) 0.45))))
   (list
    (lambda (v) (mul (adsr 0.5 0.03 0.1 1) (crush (sine (* (note v) 0.5))
						0.1 0.3)))
    (lambda (v) (mul (adsr 0 0.03 0.1 1) (mooglp (white (* 0.125 (note v)))
						 (fmod (* v 0.04) 1) 0.4)))
    (lambda (v) (mul (adsr 0 0.1 0.1 0.5)
		     (add
		      (saw (add (/ (note v) 4) (mul 1000 (pow (adsr 0.3 0.1 0 0) 3))))
		      (saw (add (+ 1 (/ (note v) 4)) (mul 1000 (pow (adsr 0.1 0.1 0 0) 3)))))))
    (lambda (v) (mul (adsr 0 0.02 0 0) (mooglp (white 4) (* v 0.01) 0.45)))))
  )

(define ss2
  (list
   (list
    (lambda (v) (moogbp (mul (adsr 0 0.2 0.1 0.1) (pink 100)) (adsr 0 0.01 0.1 1) 0.3))
    (lambda (v) (mul (adsr 0 0.01 0 1) (white 20)))
    (lambda (v) (mul (adsr 0 0.1 0.1 1) (pink 50)))
    (lambda (v) (mul (adsr 0 0.1 0.1 1) (sine (mul (adsr 0 0.1 0 0) 150)))))
   (list
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (white (+ 440 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (white (+ 240 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (white (+ 140 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (white (mul (adsr 0 0.1 0 0) (+ 40 (* v 50)))))))
   (list
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (saw (+ 440 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (saw (+ 240 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (saw (+ 140 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (saw (mul (adsr 0 0.1 0 0) (+ 40 (* v 50)))))))
   (list
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (pink (+ 440 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (pink (+ 240 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (pink (+ 140 (* v 20)))))
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (pink (mul (adsr 0 0.1 0 0) (+ 40 (* v 50)))))))

   ))


(define z (build-nz l ss 0.2))

(every-frame (nz-tick z))


