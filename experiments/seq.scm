; lz/nz
(synth-init 50 44100)

(define (lz pat)
  (vector pat 0))

;; . . . . .
;;       . . . . .
;;             . . . .
;; . . .       . .
;;

(define (lz-pat l) (vector-ref l 0))
(define (lz-pos l) (vector-ref l 1))
(define (lz-inc-pos! l)
  (vector-set! l 1 (+ (lz-pos l) 1)))
(define (lz-set-pat! l v) (vector-set! l 0 v))

(define (safe-ref l i)
  (cond
   ((< i 0) #\.)
   ((>= i (length l)) #\.)
   (else (list-ref l i))))

(define (lz-tick l)
  (let ((a (safe-ref (list-ref (lz-pat l) 0) (modulo (lz-pos l) 8)))
        (b (safe-ref (list-ref (lz-pat l) 1) (modulo (- (lz-pos l) 2) 8)))
        (c (safe-ref (list-ref (lz-pat l) 2) (modulo (- (lz-pos l) 4) 8)))
        (d (safe-ref (list-ref (lz-pat l) 3) (modulo (- (lz-pos l) 6) 8))))

    (lz-inc-pos! l)
    (list a b c d)))

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
  (make-nz lz '(40) 0 sz (ntp-time-add (t) 5) tk 1.0))

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

(define note-dec 1)
(define random-drop-off #t)
(define humanize #t)

(define (nz-tick nz)
  (when (ntp>? (ntp-time-add (t) (nz-off nz)) (nz-cur-t nz))
        (let ((t (lz-tick (nz-lz nz)))
              (v (car (nz-vals nz))))
          (set-nz-cur-t! nz (ntp-time-add (nz-cur-t nz) (nz-tk nz)))
          (let ((ht (nz-cur-t nz)))
            (for-each
             (lambda (t)
               (when (or random-drop-off (< (random 10) 8))
                     (define hht ht)
                     (when humanize (set! hht (ntp-time-add ht (* (rndf) 0.03))))

                     (cond
                      ((char=? t #\+) (set-nz-vals! nz (cons (+ (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
                      ((char=? t #\-) (set-nz-vals! nz (cons (- (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
                      ((char=? t #\<) (set-nz-vx! nz (modulo (- (nz-vx nz) 1) (length (nz-sz nz)))))
                      ((char=? t #\>) (set-nz-vx! nz (modulo (+ (nz-vx nz) 1) (length (nz-sz nz)))))
                      ((char=? t #\a) (play hht ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 0) v) 0))
                      ((char=? t #\b) (play hht ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 1) v) 0))
                      ((char=? t #\c) (play hht ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 2) v) 0))
                      ((char=? t #\d) (play hht ((list-ref (list-ref (nz-sz nz) (nz-vx nz)) 3) v) 0))
                      ((char=? t #\[) (nz-dup! nz))
                      ((char=? t #\]) (nz-pop! nz))))
               (set! v (- v note-dec))
               )
             t)))))

; --
(define l (lz (list (string->list ".b..b")
                    (string->list ".aa..")
                    (string->list "...b.")
                    (string->list "aaa.a"))))


(define ss
  (list
   (list
    (lambda (v) (mul (adsr 0 0.01 0.1 1) (sine (add (mul 20 (sine 4)) (note v)))))
    (lambda (v) (mul (adsr 0 0.1 0 0) (mul 0.2 (add (saw (* 1.5 (note v)))
						    (saw (note v)))))))
   (list
    (lambda (v) (mul (adsr 0 0.03 0.1 1) (mooghp (saw (* (note v) 0.5))
						 (mul 0.2 (adsr 0.5 0 0 0)) 0.45)))
    (lambda (v) (mul (adsr 0 0.1 0.1 1) (mooglp (add (saw (* 1.5 (note v))) (saw (note v)))
						(* v 0.12) 0.4))))
   (list
    (lambda (n) (mul (adsr 0 0.1 0 0)
                     (moogbp
                      (add (saw (note n)) (saw (* 0.333333 (note n))))
                      (adsr 0 0.1 0 0) 0.3)))
    (lambda (n) (mul (adsr 0 0.1 0 0) (mooglp (squ (* 0.25 (note n)))
                                              (adsr 0.1 0 0 0) 0.4))))
   (list
    (lambda (n) (mul (adsr 0 0.1 0.1 1)
                     (crush (sine (add (mul 100 (sine 0.3)) (note n))) 5 0.6)))
    (lambda (n) (mul (adsr 0 0.1 0 0) (moogbp
                                       (add (saw (note n)) (saw (* 0.333333 (note n))))
                                       (* 0.1 (random 10)) 0.48))))
   (list
    (lambda (n) (mul (adsr 0 0.1 0.05 1)
                     (sine
                      (add (mul 1000 (sine (* 0.3333 (note n)))) (note n)))))
    (lambda (n) (mul (adsr 0 0.1 0.05 1) (sine
                                          (add (mul 100 (sine (* 0.3333 (note n)))) (note n))))))

   (list
    (lambda (n) (mul (adsr 0 0.1 0.05 1)
                     (mul (sine (* 0.3333 (note n)))
                          (sine (note n)))))
    (lambda (n) (mul (adsr 0 0.1 0 1)
                     (mul (sine (* 1.3333 (note n)))
                          (sine (note n))))))

   (list
    (lambda (n) (mul (adsr 0 0.1 0.05 1)
                     (mul (saw (* 0.3333 (note n)))
                          (saw (note n)))))
    (lambda (n) (mul (adsr 0 0.1 0 1)
                     (mul (saw (* 1.3333 (note n)))
                          (saw (note n))))))

   (list
    (lambda (n) (mul (adsr 0 0.1 0.05 1)
                     (mul (squ (* 0.3333 (note n)))
                          (sine (note n)))))
    (lambda (n) (mul (adsr 0 0.1 0 1)
                     (mul (squ (* 1.3333 (note n)))
                          (sine (note n))))))

   )
  )

;(define z (build-nz (vector 9 5 '((4 2) (4 1) (6 0) (3 2) (4 1) (6 0)) 8 3 (list->vector (string->list "BaaadBdcd--C+++ --Aba+dd        "))) ss 0.2))

(define z (build-nz l ss 0.2))

(define minor '(2 1 2 2 1 2 2))
(define major '(2 2 1 2 2 2 1))

(define (update a b c d e)
  (lz-set-pat! l (list (map (lambda (i) (if (eqv? i 0) #\. #\b)) a)
                       (map (lambda (i) (if (eqv? i 0) #\. #\a)) b)
                       (map (lambda (i) (if (eqv? i 0) #\. #\b)) c)
                       (map (lambda (i) (if (eqv? i 0) #\. #\a)) d)))
  (if (eqv? (list-ref e 0) 0)
      (set-scale minor)
      (set-scale major))

  (set! note-dec (list-ref e 1))

  (if (eqv? (list-ref e 2) 0)
      (set! random-drop-off #f)
      (set! random-drop-off #t))

  (if (eqv? (list-ref e 3) 0)
      (set! humanize #f)
      (set! humanize #t))

  (set-nz-vals! z (list (list-ref e 4)))

  )

;;
(set-nz-vx! z (modulo (- (nz-vx z) 1) 8))

(every-frame (nz-tick z))

(update (list 1 0 0 0 1)
        (list 0 1 1 1 0)
        (list 1 0 1 0 0)
        (list 0 1 0 1 0)
        (list 3 4 5 6 7))
