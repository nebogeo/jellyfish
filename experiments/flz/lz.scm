; lz/nz
(synth-init "fluxa" 3 44100 2048 20)

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

;; md=max depth, w h - rule size * rule count
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

;; recursively step until no more jump/return stuff is happening
(define (lz-step lz)
  (let ((pos (car (lz-top lz)))
        (pat (cadr (lz-top lz)))
        (data (lz-read lz)))
    ;;(msg pos pat data (lz-stk lz))
    (cond
     ;; space is the end of a rule, so now pop or reset (also check bound)
     ((or (char=? data #\ ) (>= (car (lz-top lz)) 8))
      (cond
       ;; no more on the stack, goto 0
       ((eqv? (length (lz-stk lz)) 1)
        (set-lz-top! lz (list 0 0)))
       (else
        (lz-pop! lz)))
      (lz-step lz))
     
     ;; step to the next for return, then push new rule position
     ((char=? data #\A) (lz-inc-pos lz) (lz-push! lz (list 0 0)) (lz-step lz))
     ((char=? data #\B) (lz-inc-pos lz) (lz-push! lz (list 0 1)) (lz-step lz))
     ((char=? data #\C) (lz-inc-pos lz) (lz-push! lz (list 0 2)) (lz-step lz))
     ((char=? data #\D) (lz-inc-pos lz) (lz-push! lz (list 0 3)) (lz-step lz)))))

(define (lz-tick lz)
  (lz-step lz)
  (let ((r (lz-read lz)))
    (lz-inc-pos lz)
    r))

(define (lz-prog lz pat str)
  (let ((c 0))
    (for-each
     (lambda (item)
       (lz-poke lz c pat item)
       (set! c (+ c 1)))
     (string->list str))))

(define (lz-tests)
  (msg "testing lz")
  (let ((l (build-lz 9 8 4)))
    (lz-prog l 0 "a")
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 1 fail"))
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 2 fail"))
    (lz-prog l 0 "ab")
    (when (not (equal? (lz-tick l) #\b)) (msg "lz test 3 fail"))
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 4 fail"))
    (lz-prog l 0 "BB")
    (lz-prog l 1 "ab")
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 5 fail"))
    (when (not (equal? (lz-tick l) #\b)) (msg "lz test 6 fail"))
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 5 fail"))
    (when (not (equal? (lz-tick l) #\b)) (msg "lz test 6 fail"))
    (lz-prog l 0 "BCD")
    (lz-prog l 1 "a ")
    (lz-prog l 2 "b ")
    (lz-prog l 3 "c ")
    (lz-tick l)
    (lz-tick l)
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 7 fail"))
    (when (not (equal? (lz-tick l) #\b)) (msg "lz test 8 fail"))
    (when (not (equal? (lz-tick l) #\c)) (msg "lz test 9 fail"))
    (lz-prog l 0 "B       ")
    (lz-prog l 1 "aC ")
    (lz-prog l 2 "bD ")
    (lz-prog l 3 "cA ")
    (when (not (equal? (lz-tick l) #\a)) (msg "lz test 7 fail"))
    (when (not (equal? (lz-tick l) #\b)) (msg "lz test 8 fail"))
    (when (not (equal? (lz-tick l) #\c)) (msg "lz test 9 fail"))
    ))

(lz-tests)

; nz

(define max-vals 16)

(define (make-nz lz vals vx sz cur-t tk off grp)
  (vector lz vals vx sz cur-t tk off grp))

(define (nz-lz n) (vector-ref n 0))
(define (nz-vals n) (vector-ref n 1))
(define (nz-vx n) (vector-ref n 2))
(define (nz-sz n) (vector-ref n 3))
(define (nz-cur-t n) (vector-ref n 4))
(define (nz-tk n) (vector-ref n 5))
(define (nz-off n) (vector-ref n 6))
(define (nz-grp n) (vector-ref n 7))

(define (set-nz-vals! n v) (vector-set! n 1 v))
(define (set-nz-vx! n v) (vector-set! n 2 v))
(define (set-nz-cur-t! n v) (vector-set! n 4 v))
(define (set-nz-tk! n v) (vector-set! n 5 v))
(define (set-nz-off! n v) (vector-set! n 6 v))
(define (set-nz-grp! n v) (vector-set! n 7 v))

(define (build-nz lz sz tk)
  (make-nz lz '(20) 0 sz (ntp-time) tk 0.0 0))

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

(define (nz-dump nz c)
  (when (not (zero? c))
        (display (lz-tick (nz-lz nz)))
        (nz-dump nz (- c 1))))

; figures out the offset to the nearest tick
(define (calc-offset time-now sync-time beat-dur)
  ;; find the difference in terms of tempo
  (let* ((diff (/ (ntp-time-diff sync-time time-now) beat-dur))
         ;; get the fractional remainder (doesn't matter how
         ;; far in the past or future the synctime is)
         (fract (- diff (floor diff))))
    ;; do the snapping
    (if (< fract 0.5)
        ;; need to jump forwards - convert back into seconds
        (* fract beat-dur)
        ;; the beat is behind us, so go backwards
        (- (* (- 1 fract) beat-dur)))))

(define (bpm->seconds bpm)
  (/ 60 bpm))

(define (nz-sync nz bpm)
  (let ((new-tk (bpm->seconds bpm))
	(now (ntp-time)))
    (msg (car now) " " (cadr now) " sync")
    (set-nz-off! nz (* new-tk 4))
    (set-nz-tk! nz new-tk)
    (let ((sync (ntp-time-add 
		 (nz-cur-t nz) 
		 (- (calc-offset now (nz-cur-t nz) new-tk))
		 )))
      (msg (car sync) " " (cadr sync) " setting")
      (set-nz-cur-t! nz sync))))

(define (nz-sound nz id v d)
  (let ((group (list-ref (nz-sz nz) (modulo (nz-grp nz) (length (nz-sz nz))))))
    (let ((voices (list-ref group (modulo (nz-vx nz) (length group)))))
      ((list-ref voices id) v d))))

(define (nz-tick nz)
  (let ((now (ntp-time)))
    (let ((future (ntp-time-add now (nz-off nz))))
      (when (ntp>? future (nz-cur-t nz))
	    ;;(msg (car (nz-cur-t nz)) " " (cadr (nz-cur-t nz)) " tick-time")
	    ;;(msg (car now) " " (cadr now) " tick-real")
	    ;;(msg (car future) " " (cadr future) " tick-future")
	    (let ((t (lz-tick (nz-lz nz)))
		  (v (car (nz-vals nz))))
	      (when (or (char=? t #\a) (char=? t #\b)
	                (char=? t #\c) (char=? t #\d)
	                (char=? t #\.))
		    (set-nz-cur-t! nz (ntp-time-add (nz-cur-t nz) (nz-tk nz))))
	      (cond
	       ((char=? t #\+) (set-nz-vals! nz (cons (+ (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
	       ((char=? t #\-) (set-nz-vals! nz (cons (- (car (nz-vals nz)) 1) (cdr (nz-vals nz)))))
	       ((char=? t #\<) (set-nz-vx! nz (modulo (- (nz-vx nz) 1) (length (nz-sz nz)))))
	       ((char=? t #\>) (set-nz-vx! nz (modulo (+ (nz-vx nz) 1) (length (nz-sz nz)))))
	       ((char=? t #\a) (play (nz-cur-t nz) (nz-sound nz 0 v (nz-tk nz)) 0.2))
	       ((char=? t #\b) (play (nz-cur-t nz) (nz-sound nz 1 v (nz-tk nz)) -0.2))
	       ((char=? t #\c) (play (nz-cur-t nz) (nz-sound nz 2 v (nz-tk nz)) 0.7))
	       ((char=? t #\d) (play (nz-cur-t nz) (nz-sound nz 3 v (nz-tk nz)) -0.7))
	       ((char=? t #\[) (nz-dup! nz))
	       ((char=? t #\]) (nz-pop! nz)))
	      )))))

; --


(define ss
  (list
   (list ;; group 1 - bitty and textural
    (list ;; short test tones
     (lambda (v d) (mul (adsr 0 (* (modulo v 10) 0.01) 1 0) (white 44)))
     (lambda (v d) (mul (adsr 0 (* (modulo v 9) 0.01) 1 0) (sine 440)))
     (lambda (v d) (mul (adsr 0 (* (modulo v 11) 0.01) 1 0) (tri 440)))
     (lambda (v d) (mul (adsr 0 (* (modulo v 12) 0.01) 1 0) (pink 44))))
    (list
     (lambda (v d) (mul (adsr 0 0.1 0 0) (pink (mul (adsr 0 (* (modulo v 10) 0.2) 0 0) 1000))))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (sine (mul (adsr 0 0.2 0 0) 100))))
     (lambda (v d) (mul (adsr 0 0.2 0 0) (sine 100)))
     (lambda (v d) (echo (mul (adsr 0 (* (modulo v 10) 0.0125) 0 0) 
			      (white (mul (adsr 0 0.05 0.2 0.3) 2000)))
			 (* (modulo v 9) 0.125 (nz-tk z)) 0.95)))
    (list ;; toy drums
     (lambda (v d) (mul (adsr 0 0.05 0 1) (pink 4)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) 
			(distort 
			 (sine (mul 100 (adsr 0 0.1 0 0) 200))
			 (* (modulo v 10) 0.1))))
     (lambda (v d) (mul (adsr 0 0.2 0 1) (mooghp (white 4) (adsr 0 0.2 0 0) (* (modulo v 20) 0.05))))
     (lambda (v d) (echo (mul (adsr 0 0.01 0 1) (white 19)) 
			 (* (modulo v 9) 0.25 (nz-tk z)) 0.9)))
    )
   (list ;; group 2 - pretty ambient tunefulness

    (list ;; pretty gamelan gone wrong
     (lambda (v d) (crush (mul (adsr 0 0.01 0.1 1) 
			       (mul (sine (* (note v) 2.5))
				    (sine (note v))))
			  (* (+ 2 (modulo v 50)) 2) 0.3))
     (lambda (v d) (crush (mul (adsr 0 0.004 0.1 3) 
			       (mul (sine (* (note v) 0.75))
				    (sine (note v))))
			  (* (+ 1 (modulo v 4)) 2) 0.3))
     (lambda (v d) (crush (mul (adsr 0.05 0.02 0.1 2) 
			       (mul (sine (* (note v) 0.5))
				    (sine (note v))))
			  (* (+ 2 (modulo v 5)) 2) 0.3))
     (lambda (v d) (crush (mul (adsr 0 0.03 0.1 5) 
			       (mul (tri (* (note v) 1.001))
				    (tri (note v))))
			  (* (+ 2 (modulo v 12)) 2) 0.1)))
    
    ;;    (list ;; totally out of tune
    ;;     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (saw (+ 440 (* v 20)))))
    ;;     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (saw (+ 240 (* v 20)))))
    ;;     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (saw (+ 140 (* v 20)))))
    ;;     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (saw (mul (adsr 0 0.1 0 0) (+ 40 (* v 50)))))))
    
    (list ;; fm squelch glich slow-grow
     (lambda (v d) (mul (adsr 0 0.01 0.1 1) 
			(sine (add 
			       (mul (* v 100) 
				    (mul (adsr 0.5 0.2 0.2 0.5)
					 (sine 
					  (add (mul (* v 30) (sine (* (note v) 2.5)))
					       (* (note v) 0.2))))) 
			       (note v)))))
     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (sine (add (mul 20 (sine 40)) (note v)))))
     (lambda (v d) (mul (adsr 0 0.01 1 0.1) (crush (pink 40) 3 (* (+ (modulo v 12) 1) 0.02))))
     (lambda (v d) (mul (adsr 0 0.01 1 2) (pad (note v) (* (modulo v 5) 0.02) 0.5 0.6))))

    (list ;; basic glitch in tune
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (pink (note v))))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (sine (note v))))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (pink (note (- v 12)))))
     (lambda (v d) (echo (mul (adsr 0 0.11 0.1 1) (pink (note (+ v 12)))) 
			 (* (modulo v 9) 0.25 (nz-tk z)) 0.9)))
    
    )
   (list ;; group 3 - wonky rave
    
    (list ;; nice percs
     (lambda (v d) (moogbp (mul (adsr 0 0.2 0.1 0.1) (pink 100)) (adsr 0 0.01 0.1 1) 0.3))
     (lambda (v d) (mul (adsr 0 0.01 0 1) (white 20)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (pink 50)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (sine (mul (adsr 0 0.1 0 0) 150)))))
    
    (list ;; gamey dubstep
     (lambda (v d) (mul (adsr 0.2 0.03 0.1 1) (crush (sine (* (note v) 0.5))
						     0.1 0.3)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (mooglp (white (* 0.125 (note v)))
						   (fmod (* v 0.04) 1) 0.4)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 0.5)
			(add
			 (saw (add (/ (note v) 4) (mul 1000 (pow (adsr 0.3 0.1 0 0) 3))))
			 (saw (add (+ 1 (/ (note v) 4)) (mul 1000 (pow (adsr 0.1 0.1 0 0) 3)))))))
     (lambda (v d) (mul (adsr 0 0.02 0 0) (mooglp (white 4) (* v 0.01) 0.45))))



    (list ;; curly kick and chord
     (lambda (v d) (mul (adsr 0 0.03 0.1 1) (mooghp (saw (* (note v) 0.5))
						    (mul 0.2 (adsr 0.5 0 0 0)) 0.45)))
     (lambda (v d) (mul (adsr 0 0.1 0.1 1) (mooglp (add (saw (* 1.5 (note v))) (saw (note v)))
						   (* v 0.12) 0.4)))
     (lambda (v d) (mul (adsr 0 0.1 0 0) (sine (add
						(fmod (* v 50) 300)
						(mul 1000 (pow (adsr 0 0.2 0 0) 10))))))
     (lambda (v d) (mul (adsr 0.04 0.02 0 0) (mooglp (white 4) (* v 0.01) 0.45))))


    (list ;; basic n spooky
     (lambda (v d) (mul (adsr 0 0.01 0.1 1) (sine (add (mul 20 (sine 4)) (note v)))))
     (lambda (v d) (mul (adsr 0 0.1 0 0) (mul 0.2 (add (saw (* 1.5 (note v)))
						       (saw (note v))))))
     (lambda (v d) (mul (adsr 0 0.1 0 0)
			(moogbp (squ (add 10 (mul 1000 (pow (adsr 0 0.2 0 0) 10))))
				(* v 0.1) 0.1)))
     (lambda (v d) (mul (adsr 0 0.02 0 0) (moogbp (white 4) (* v 0.01) 0.45))))




    (list ;; hiq plucked vocal
     (lambda (v d) (echo (mul (adsr 0 0.01 0 0) (white 12)) 
			 (* (modulo v 5) 0.25 (nz-tk z)) 0.9))
     (lambda (v d) (formant (mul (adsr 0 d 0 0) 
				 (mul (add (tri 120) (tri 422.3)) 0.5)) 
			    (* (modulo v 10) 0.1) 0))
     (lambda (v d) (ks (note (/ v 4)) (+ 0.9 (* (modulo v 5) 0.002)) 0.1))
     (lambda (v d) (formant (mul (adsr 0 d 0.2 1) 
				 (mul (add (tri 12) (tri 42.3)) 0.5)) 
			    (* (modulo v 15) 0.1) 0)))



    )
   (list ;; group 4 - rave, stabs and acid
    (list ;; stabs
     (lambda (v d) (mul (adsr 0 d 0 1) 
			(moogbp 
			 (add 
			  (add (saw (note v))
			       (saw (* (note v) 1.5)))
			  (saw (* (note v) 1.02)))
			 (* (modulo v 4) 0.25)
			 0.3)))
     (lambda (v d) (mul (adsr 0 d 0.2 1) 
			(moogbp 
			 (add 
			  (add (squ (note v))
			       (squ (* (note v) 1.5)))
			  (squ (* (note v) 1.02)))
			 (* (modulo v 5) 0.2) 
			 0.4)))
     (lambda (v d) (mul (adsr 0 d 0 0) (moogbp (saw (/ (note v) 2)) (rndf) (rndf))))
     (lambda (v d) (mul (adsr 0 d 0 0) (squ (mul (adsr 0 0.1 0 0) 200))))
     )

    (list ;; random acid w kik
     (lambda (v d) (mul (adsr 0 d 0 0) (mooglp (saw (note v)) (rndf) (rndf))))
     (lambda (v d) (mul (adsr 0 d 0.1 1) (mooghp (saw (note v)) (rndf) (rndf))))
     (lambda (v d) (mul (adsr 0 d 0 0) (moogbp (saw (/ (note v) 2)) (rndf) (rndf))))
     (lambda (v d) (mul (adsr 0 d 0 0) (squ (mul (adsr 0 0.1 0 0) 200))))
     )


    )
   (list ;; group 4 - aljazari
    
    (list
     (lambda (n d) (mul 1 (sample (get-sample n samples) 440)))
     (lambda (n d) (mul (adsr 0 0.1 0 0) (moogbp
					  (add (saw (note n)) (saw (* 0.333333 (note n))))
					  (adsr 0 0.1 0 0) 0.3)))
     (lambda (n d) (mul (adsr 0 0.1 0 0) (mooglp (squ (* 0.25 (note n)))
						 (adsr 0.1 0 0 0) 0.4)))
     (lambda (n d) (mul (adsr 0 0.1 0.05 1) (sine
					     (add (mul 100 (sine (* 0.3333 (note n)))) (note n))))))
    
    
    (list
     (lambda (n d) (mul 1 (sample (get-sample n samples) 440)))
     (lambda (n d) (mul (adsr 0 0.1 0 0)
			(mul (saw (note n)) (sine (mul (mul 0.1 (adsr 0.4 0.3 0 0)) (note n))))))
     (lambda (n d) (mul (adsr 0 0.1 0.05 1) (sine
					     (add (mul (mul 1000 (adsr 0 0.1 0.3 1))
						       (sine (* 0.3333 (note n)))) (note n)))))
     (lambda (n d) (mul (adsr 0 0.1 0.05 1) (moogbp
					     (add (saw (note n)) (saw (* 0.333333 (note n))))
					     (* 0.1 (random 10)) 0.48))))
    (list
     (lambda (n d) (mul 1 (sample (get-sample n samples) 440)))
     (lambda (n d) (mul (adsr 0 0.1 0.1 1)
			(crush (sine (add (mul 100 (sine 0.3)) (note n))) 5 0.6)))
     (lambda (n d) (mul (adsr 0 0.1 0 0) (moogbp
					  (add (saw (note n)) (saw (* 0.333333 (note n))))
					  (* 0.1 (random 10)) 0.48)))
     (lambda (n d) (mul (adsr 0 0.1 0.05 1) (sine
					     (add (mul 1000 (sine (* 0.3333 (note n)))) (note n))))))

    
    )))

;; ideas - fm zone
;; noise grind
;; no control echo zone
;; more pads
;; 4 chord progression
;; ks effect
;; pow/th/sh??

(define (sample n) (mul (adsr 0.4 0.2 0 0) (sine n)))
(define (get-sample n samples) (note n))
(define samples 0)

(define l (build-lz 10 8 4))

(lz-prog l 0 "ad-B+")
(lz-prog l 1 "ad+C-")
(lz-prog l 2 "ab+D-")
(lz-prog l 3 "++A--")


					;(define z (build-nz (vector 9 5 '((4 2) (4 1) (6 0) (3 2) (4 1) (6 0)) 8 3 (list->vector (string->list "BaaadBdcd--C+++ --Aba+dd        "))) ss 0.2))

(define z (build-nz l ss 0.2))

;; control blocks needed
;; ---------------------
;; chord 
;; voice group
;; bar sync lock

;;(set-scale '(4 4 1 1 2))
(set-scale '(1 1 1 1 1 1 1 1 1 1 1 1))

(nz-dump z 1000)

(set-nz-grp! z 3)
(set-nz-vx! z 0)

(every-frame (nz-tick z))

;;(synth-record "outz")
