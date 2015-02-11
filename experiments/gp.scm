(define max-depth 22)

(define (arg-desc type lo hi) (list type lo hi))
(define (arg-desc-type a) (list-ref a 0))
(define (arg-desc-lo a) (list-ref a 1))
(define (arg-desc-hi a) (list-ref a 2))

(define (function-desc name arg-desc-list) (list name arg-desc-list))
(define (function-desc-name a) (list-ref a 0))
(define (function-desc-arg-desc-list a) (list-ref a 1))

(define (rand-range lo hi)
  (+ lo (* (random (* (- hi lo) 100000)) 0.00001)))

(define (rand-pick l)
  (list-ref l (random (length l))))

(define (make-arg arg-desc vocab depth)
  (cond
    ((and (< depth max-depth) (zero? (random 2))) ; function
     (make-function vocab (+ depth 1)))
    (else ; terminal
     (cond
       ((eq? 'float (arg-desc-type arg-desc))
        (rand-range (arg-desc-lo arg-desc) (arg-desc-hi arg-desc)))
       ((eq? 'string (arg-desc-type arg-desc))
        "HANDCLP1.WAV")
       (else (error))))))

(define (gp-make-args arg-desc-list vocab out depth)
  (cond
    ((null? arg-desc-list) out)
    (else
     (cons (make-arg (car arg-desc-list) vocab (+ depth 1))
           (gp-make-args (cdr arg-desc-list) vocab out (+ depth 1))))))

(define (make-function-inner function-desc vocab depth)
  (cons (function-desc-name function-desc)
        (gp-make-args (function-desc-arg-desc-list function-desc) vocab '() (+ depth 1))))

(define (make-function vocab depth)
  (make-function-inner (rand-pick vocab) vocab depth))

;----------------------------------------------------------------------

(define (mutate-args rate vocab args-list depth)
  (cond
    ((null? args-list) '())
    ((> depth max-depth) ; just copy if too big
     (cons (car args-list)
           (mutate-args rate vocab (cdr args-list) (+ depth 1))))
    (else
     (cond
       ((list? (car args-list))
        (cons (mutate-function-inner rate vocab (car args-list) (+ depth 1))
              (mutate-args rate vocab (cdr args-list) (+ depth 1))))
       (else
        (if (zero? (random rate))
            (cons (make-function vocab depth)
                  (mutate-args rate vocab (cdr args-list) (+ depth 1)))
            (cons (car args-list)
                  (mutate-args rate vocab (cdr args-list) (+ depth 1)))))))))

(define (mutate-function-inner rate vocab function depth)
  (cond
    ((null? function) (error))
    (else
     (if (zero? (random rate))
         (make-function vocab depth)
         (cons (car function) (mutate-args rate vocab (cdr function) (+ depth 1)))))))

(define (mutate-function rate vocab function)
  (mutate-function-inner rate vocab function 0))

;------------------------------------------------------------------------------

; make an initial population by mutating an individual
(define (make-population size function rate vocab)
    (build-list size (lambda (n) (mutate-function rate vocab function))))

; returns a mutated version of this population
(define (mutate-population population rate vocab)
    (map
        (lambda (function)
            (mutate-function rate vocab function))
        population))

; gets a list of fitnesses of a population, using the supplied fitness function
(define (fitness-list population fitness-proc fitness-user-data)
    (map
        (lambda (function)
            (fitness-proc function fitness-user-data))
        population))

; helpers to get the min and max of a list
(define (list-max l)
    (foldl
        (lambda (e t)
            (if (> e t) e t))
        0
        l))

(define (list-min l)
    (foldl
        (lambda (e t)
            (if (< e t) e t))
        999999999
        l))

; culls the bottom part of the population (death)
(define (cull population fitlist score)
    (let* ((max (list-max fitlist))
            (min (list-min fitlist))
            (cutoff (+ min (* score (- max min)))))
        (foldl
            (lambda (i f r)
                (if (>= f cutoff)
                    (cons i r) r))
            '()
            population
            fitlist)))

; makes a new population by mutation of individuals
(define (new-population rate vocab source-pop size)
    (if (null? source-pop)
        '()
        (build-list size
            (lambda (n)
                (mutate-function rate vocab (rand-pick source-pop))))))

(define (fittest l)
    (cadr
        (foldl
            (lambda (i r)
                (if (> i (caddr r))
                    (list (+ (car r) 1) (car r) i)
                    (list (+ (car r) 1) (cadr r) (caddr r))))
            (list 0 0 0) ; current, fittest, fitness
            l)))

;-----------------------------------------------------------------

(define vocab
  (list
  (function-desc 'sine (list (arg-desc 'float 0 1000)))
  (function-desc 'saw (list (arg-desc 'float 0 1000)))
  (function-desc 'tri (list (arg-desc 'float 0 1000)))
  (function-desc 'squ (list (arg-desc 'float 0 1000)))
  (function-desc 'white (list (arg-desc 'float 0 1000)))
  (function-desc 'pink (list (arg-desc 'float 0 1000)))
  (function-desc 'adsr (list (arg-desc 'float 0 1) (arg-desc 'float 0 1)
                             (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'add (list (arg-desc 'float 0 1000) (arg-desc 'float 0 1000)))
  (function-desc 'sub (list (arg-desc 'float 0 1000) (arg-desc 'float 0 1000)))
  (function-desc 'mul (list (arg-desc 'float 0 1000) (arg-desc 'float 0 1000)))
;;  (function-desc 'div (list (arg-desc 'float -1000 1000) (arg-desc 'float -1000 1000)))
;;  (function-desc 'pow (list (arg-desc 'float -1000 1000) (arg-desc 'float -1000 1000)))
  (function-desc 'mooglp (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'moogbp (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'mooghp (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'formant (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'crush (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'distort (list (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'klip (list (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'echo (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'ks (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'xfade (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 's&h (list (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 't&h (list (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
  (function-desc 'pad (list (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1) (arg-desc 'float 0 1)))
))

(random 13)

(synth-init)

;(play-now (tri 292) 0)

(define (loop n)
  (when (not (zero? n))
        (sqrt 1)
        (loop (- n 1))))

(define (trig)
  (let ((p (make-function vocab 0)))
    (display p)(newline)
    (play-now (eval p) 0))
  (loop 9999)
  (trig))

(trig)
