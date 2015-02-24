;; [ Copyright (C) 2015 Dave Griffiths : GPLv2 see LICENCE ]

; an experimental non-determinsistic synth

(define op_terminal 0) (define op_sine 1) (define op_saw 2) (define op_tri 3) (define op_squ 4)
(define op_white 5) (define op_pink 6) (define op_adsr 7) (define op_add 8) (define op_sub 9)
(define op_mul 10) (define op_div 11) (define op_pow 12) (define op_mooglp 13) (define op_moogbp 14)
(define op_mooghp 15) (define op_formant 16) (define op_sample 17) (define op_crush 18)
(define op_distort 19) (define op_clip 20) (define op_echo 21) (define op_ks 22) (define op_xfade 23)
(define op_sampnhold 24) (define op_tracknhold 25) (define op_pad 26) (define op_cryptodistort 27)

;;---------------------------------------------------------------
;; language - building graph construction commands from syntax

(define current-id 0)

(define (new-id)
  (let ((ret (+ current-id 1)))
    (set! current-id ret)
    ret))

(define (node id) (list "node" id))
(define (node-id n) (cadr n))
(define (node? n)
  (and (list? n) (not (null? n)) (equal? (car n) "node")))

(define (get-node-id v)
  (cond ((node? v)
         (node-id v))
        (else
         (let ((id (new-id)))
           (synth-create id op_TERMINAL v)
           id))))

(define (make-args id operands)
  (let ((index -1))
    (foldl
     (lambda (a l)
       (set! index (+ index 1))
       (append l (list (list id index (get-node-id a)))))
     '()
     operands)))

(define (operator op operands)
  (let ((id (new-id)))
    (synth-create id op 0)
    (map (lambda (l) (apply synth-connect l)) (make-args id operands))
    (node id)))

(define (play-now node pan) (synth-play 0 0 (node-id node) pan))
(define (play time node pan) (synth-play (car time) (cadr time) (node-id node) pan))

;;---------------------------------------------------------------
;; operators

(define (sine a) (operator op_sine (list a)))
(define (saw a) (operator op_saw (list a)))
(define (tri a) (operator op_tri (list a)))
(define (squ a) (operator op_squ (list a)))
(define (white a) (operator op_white (list a)))
(define (pink a) (operator op_pink (list a)))

(define (add a b) (operator op_add (list a b)))
(define (sub a b) (operator op_sub (list a b)))
(define (mul a b) (operator op_mul (list a b)))
(define (div a b) (operator op_div (list a b)))
(define (pow a b) (operator op_pow (list a b)))

(define (adsr a d s r) (operator op_adsr (list a d s r)))

(define (mooglp in c r) (operator op_mooglp (list in c r)))
(define (moogbp in c r) (operator op_moogbp (list in c r)))
(define (mooghp in c r) (operator op_mooghp (list in c r)))
(define (formant in c r) (operator op_formant (list in c r)))

(define (crush in freq bits) (operator op_crush (list in bits freq)))
(define (distort in amount) (operator op_distort (list in amount)))
(define (klip in amount) (operator op_clip (list in amount)))
(define (echo in delaytime feedback) (operator op_echo (list in delaytime feedback)))
(define (ks freq cutoff resonance) (operator op_ks (list freq cutoff resonance)))
(define (xfade s0 s1 mix) (operator op_xfade (list s0 s1 mix)))
(define (s&h sig cv) (operator op_sampnhold (list sig cv)))
(define (t&h sig cv) (operator op_tracknhold (list sig cv)))
(define (pad a g c r) (operator op_pad (list a g c r)))

;;---------------------------------------------------------------
;; tuning

(define (set-scale s) (set! flx-scale s))

(define flx-scale '(1 1 1 1 1 1 1 1 1 1 1))

;; just intonation (erm I think...)
(define scale-lut (list 58.2705 61.7354 65.4064 69.2957 73.4162 77.7817
                        82.4069 87.3071 92.4986 97.9989 103.826 110 116.541 123.471 130.813 138.591
                        146.832 155.563 164.814 174.614 184.997 195.998 207.652 220 233.082 246.942
                        261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440
                        466.164 493.883 523.251 554.365 587.33 622.254 659.255 698.456 739.989 783.991
                        830.609 880 932.328 987.767 1046.5 1108.73 1174.66 1244.51 1318.51 1396.91
                        1479.98 1567.98 1661.22 1760 1864.66 1975.53 2093 2217.46 2349.32 2489.02
                        2637.02 2793.83 2959.96 3135.96 3322.44 3520 3729.31 3951.07 4186.01 4434.92
                        4698.64 4978.03 5274.04 5587.65 5919.91 6271.93 6644.88 7040 7458.62 7902.13
                        8372.02 8869.84 9397.27 9956.06 10548.1 11175.3 11839.8 12543.9 13289.8 14080
                        14917.2 15804.3 16744 17739.7 18794.5 19912.1 21096.2 22350.6 23679.6 25087.7
                        26579.5 28160 29834.5 31608.5 33488.1 35479.4 37589.1 39824.3 42192.3 44701.2
                        47359.3 50175.4 53159 56320))

(define (pick l c)
  (list-ref l (modulo c (length l))))

(define (inter l c)
  (define (_ l c cc a)
    (cond ((eqv? c cc) a)
          (else
           (_ l c (+ cc 1) (+ a (pick l cc))))))
  (_ l c 0 0))

(define (note n)
  (list-ref scale-lut (modulo (inter flx-scale (abs (inexact->exact (round n)))) (length scale-lut))))

;;---------------------------------------------------------------
