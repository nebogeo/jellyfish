(define op_terminal 0) (define op_sine 1) (define op_saw 2) (define op_tri 3) (define op_squ 4)
(define op_white 5) (define op_pink 6) (define op_adsr 7) (define op_add 8) (define op_sub 9)
(define op_mul 10) (define op_div 11) (define op_pow 12) (define op_mooglp 13) (define op_moogbp 14)
(define op_mooghp 15) (define op_formant 16) (define op_sample 17) (define op_crush 18)
(define op_distort 19) (define op_clip 20) (define op_echo 21) (define op_ks 22) (define op_xfade 23)
(define op_sampnhold 24) (define op_tracknhold 25) (define op_pad 26) (define op_cryptodistort 27)

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

(define (play-now node pan) (synth-play 0 (node-id node) pan))

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
