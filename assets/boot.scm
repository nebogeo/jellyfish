;; [ Copyright (C) 2011 Dave Griffiths : GPLv2 see LICENCE ]

(define frame-thunk '())
(define flx_time 0)
(define _touching #f)
(define _touches '())
(define _fling (vector 0 0))

(define-macro (every-frame . args)
  `(begin (set! frame-thunk (lambda () ,@args))))

(define (frame-hook)
  (set! flx_time (+ flx_time 1))
  (if (not (null? frame-thunk))
      (frame-thunk))
  (set! _touches '())
  (set! _touching #f)
  (vector-set! _fling 0 0)
  (vector-set! _fling 1 0))

(define triangles 0)
(define triangle-strip 1)

(define (hint-none) (hint 0))
(define (hint-solid) (hint 1))
(define (hint-wire) (hint 2))
(define (hint-normal) (hint 3))
(define (hint-points) (hint 4))
(define (hint-anti-alias) (hint 5))
(define (hint-bound) (hint 6))
(define (hint-unlit) (hint 7))

;------------------------------------------------------------

(define _mouse-x 0)
(define _mouse-y 0)
(define _mouse-b -1)
(define _mouse-s 1) ; state - 0 down, 1 up

(define (input-mouse-button b s)
  (set! _mouse-b b)
  (set! _mouse-s s))

(define (input-mouse x y)
  (when (zero? _mouse-s) ; eh?
        (set! _touching #t)
        (set! _touches (list (list 0 x y))))
  (set! _mouse-x x)
  (set! _mouse-y y))

(define (mouse-x) _mouse-x)
(define (mouse-y) _mouse-y)
(define (mouse-button n)
  (if _touching
      #t
      (if (zero? _mouse-s)
          (eqv? _mouse-b n) #f)))

(define keys '())
(define keys-this-frame '())
(define special-keys '())
(define special-keys-this-frame '())
(define mouse (vector 0 0))
(define mouse-buttons (vector #f #f #f))
(define mouse-wheel-v 0)
(define key-mods '())

; utils funcs for using lists as sets
(define (set-remove a l)
  (if (null? l)
      '()
      (if (eq? (car l) a)
          (set-remove a (cdr l))
          (cons (car l) (set-remove a (cdr l))))))

(define (set-add a l)
  (if (not (memq a l))
      (cons a l)
      l))

(define (set-contains a l)
  (if (not (memq a l))
      #f
      #t))

(define (clear-down)
  (set! keys '()))

(define (update-input)
  (set! keys-this-frame '())
  (set! special-keys-this-frame '())
  (set! mouse-wheel-v 0))

(define (register-down key button special state x y mod)
  (when (not (or (number? key) (eq? key -1))) ; ordinary keypress
    (set! keys (set-add key keys))
	(set! keys-this-frame (set-add key keys-this-frame)))
  (when (not (= special -1)) ; special keypress
    (set! special-keys (set-add special special-keys))
	(set! special-keys-this-frame (set-add special special-keys-this-frame)))

  ;(set! key-mods                        ; key modifiers
  ;      (for/list ([bitmask (list 1 2 4)]
  ;                 [bitsym '(shift ctrl alt)]
  ; #:when (> (bitwise-and mod bitmask) 0))
  ;                bitsym))
  (cond ; mouse
   ((and (eq? key 0) (eq? special -1))
    (when (eq? button 3) (set! mouse-wheel-v 1))
    (when (eq? button 4) (set! mouse-wheel-v -1))
    (when (and (eq? state 0)
               (< button (vector-length mouse-buttons)))
          (vector-set! mouse-buttons button #t))
    (when (and (eq? state 1)
               (< button (vector-length mouse-buttons)))
          (vector-set! mouse-buttons button #f))
    (vector-set! mouse 0 x)
    (vector-set! mouse 1 y))))

(define (register-up key button special state x y mod)
  (when (not (eq? key -1))
    (set! keys (set-remove key keys)))
  (when (not (eq? special -1))
    (set! special-keys (set-remove special special-keys))))

(define (key-pressed s)
  (set-contains (car (string->list s)) keys))

(define (keys-down)
  keys)

(define (key-special-pressed k)
  (set-contains k special-keys))

(define (keys-special-down)
  special-keys)

(define (key-modifiers)
  key-mods)

(define (key-pressed-this-frame s)
  (set-contains (car (string->list s)) keys-this-frame))

(define (key-special-pressed-this-frame s)
  (set-contains s special-keys-this-frame))

(define (fluxus-input-callback key button special state x y mod)
  (register-down key button special state x y mod)
  (input-camera key button special state x y mod width height))

(define (fluxus-input-release-callback key button special state x y mod)
  (register-up key button special state x y mod))



;------------------------------------------------------------


(define (input-touches l)
  (set! _touching #t)
  (input-mouse (list-ref (car l) 1)
               (list-ref (car l) 2))
  (set! _touches l))

(define on-sensor (lambda (x y z) 0))

(define (input-sensor l)
  (on-sensor (list-ref l 0) (list-ref l 1) (list-ref l 2) ))

(define (get-touch-ids)
  (map
   (lambda (touch)
     (car touch))
   _touches))

(define (get-pos-from-touch id)
  (foldr
   (lambda (r touch)
     (if (eq? (car touch) id)
         (cdr touch)
         r))
   '(0 0)
   _touches))

(define (on-fling vx vy)
  (vector-set! _fling 0 vx)
  (vector-set! _fling 1 vy))


;------------------------------------------------------------

(define (time) flx_time)

(define (build-locator)
  (build-polygons 0 0))

(define-macro (with-state . args)
  `(begin (push) (let ((r (begin ,@args))) (pop) r)))

(define-macro (with-primitive . args)
  (let ((id (car args)) (body (cdr args)))
    `(begin (grab ,id) (let ((r (begin ,@body))) (ungrab) r))))

(define (build-list fn n)
  (define (_ fn n l)
    (cond ((zero? n) l)
          (else
           (_ fn (- n 1) (cons (fn (- n 1)) l)))))
  (_ fn n '()))

(define (foldl op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result) (cdr rest))))
  (iter initial seq))

(define (filter op seq)
  (foldl
   (lambda (i s)
     (if (op i) (cons i s) s))
   '()
   seq))

;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)
(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))


(define (square x)
  (* x x))

(define (vx v) (vector-ref v 0))
(define (vy v) (vector-ref v 1))
(define (vz v) (vector-ref v 2))

(define (vadd a b)
  (vector (+ (vx a) (vx b))
          (+ (vy a) (vy b))
          (+ (vz a) (vz b))))

(define (vmag v)
  (sqrt (+ (square (vx v))
           (square (vy v))
           (square (vz v)))))

(define (vsub a b)
  (vector (- (vx a) (vx b))
          (- (vy a) (vy b))
          (- (vz a) (vz b))))

(define (vmul v a)
  (vector (* (vx v) a) (* (vy v) a) (* (vz v) a)))

(define (vdiv v a)
  (vector (/ (vx v) a) (/ (vy v) a) (/ (vz v) a)))

(define (vdist a b)
  (vmag (vsub a b)))

(define (vlerp v1 v2 t)
	(vadd v1 (vmul (vsub v2 v1) t)))

(define (vnormalise v)
  (vdiv v (vmag v)))

(define (pdata-map! . args)
  (let ((proc (car args))
        (pdata-write-name (cadr args))
        (pdata-read-names (cddr args)))
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set!
                          pdata-write-name n
                          (apply
                           proc
                           (cons
                            (pdata-ref pdata-write-name n)
                            (map
                             (lambda (read)
                               (pdata-ref read n))
                             pdata-read-names))))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1)))))

(define (pdata-index-map! . args)
  (let ((proc (car args))
        (pdata-write-name (cadr args))
        (pdata-read-names (cddr args)))
     (letrec
         ((loop (lambda (n total)
                  (cond ((not (> n total))
                         (pdata-set!
                          pdata-write-name n
                          (apply
                           proc
                           (append
                            (list
                             n
                             (pdata-ref pdata-write-name n))
                            (map
                             (lambda (read)
                               (pdata-ref read n))
                             pdata-read-names))))
                         (loop (+ n 1) total))))))
       (loop 0 (- (pdata-size) 1)))))

(define (pdata-copy a b)
  (pdata-add b)
  (pdata-map! (lambda (b a) a) b a))

(define (mscale v)
  (vector (vx v) 0 0 0
          0 (vy v) 0 0
          0 0 (vz v) 0
          0 0 0 1))

; 0  1  2  3
; 4  5  6  7
; 8  9 10 11
;12 13 14 15

(define (mtranspose m)
  (vector (vector-ref m 0) (vector-ref m 4) (vector-ref m 8) (vector-ref m 12)
          (vector-ref m 1) (vector-ref m 5) (vector-ref m 9) (vector-ref m 13)
          (vector-ref m 2) (vector-ref m 6) (vector-ref m 10) (vector-ref m 14)
          (vector-ref m 3) (vector-ref m 7) (vector-ref m 11) (vector-ref m 15)))


(define (vtransform v m)
  (let ((m m));(mtranspose m)))
    (let ((w (+ (* (vx v) (vector-ref m 3))
                (* (vy v) (vector-ref m 7))
                (* (vz v) (vector-ref m 11))
                (vector-ref m 15))))
   (vdiv
    (vector
     (+ (* (vx v) (vector-ref m 0))
        (* (vy v) (vector-ref m 4))
        (* (vz v) (vector-ref m 8))
        (vector-ref m 12))
     (+ (* (vx v) (vector-ref m 1))
        (* (vy v) (vector-ref m 5))
        (* (vz v) (vector-ref m 9))
        (vector-ref m 13))
     (+ (* (vx v) (vector-ref m 2))
        (* (vy v) (vector-ref m 6))
        (* (vz v) (vector-ref m 10))
        (vector-ref m 14)))
    w))))

;------------------------------------------------------------

(define random-maker
  (let* ((multiplier 48271)
         (modulus 2147483647)
         (apply-congruence
          (lambda (current-seed)
            (let ((candidate (modulo (* current-seed multiplier)
                                     modulus)))
              (if (zero? candidate)
                  modulus
                  candidate))))
         (coerce
          (lambda (proposed-seed)
            (if (integer? proposed-seed)
                (- modulus (modulo proposed-seed modulus))
                19860617))))  ;; an arbitrarily chosen birthday
  (lambda (initial-seed)
    (let ((seed (coerce initial-seed)))
      (lambda args
        (cond ((null? args)
               (set! seed (apply-congruence seed))
               (/ (- modulus seed) modulus))
              ((null? (cdr args))
               (let* ((proposed-top
                       (ceiling (abs (car args))))
                      (exact-top
                       (if (inexact? proposed-top)
                           (inexact->exact proposed-top)
                           proposed-top))
                      (top
                       (if (zero? exact-top)
                           1
                           exact-top)))
                 (set! seed (apply-congruence seed))
                 (inexact->exact (floor (* top (/ seed modulus))))))
              ((eq? (cadr args) 'reset)
               (set! seed (coerce (car args))))
              (else
               (display "random: unrecognized message")
               (newline))))))))

(define rand
  (random-maker 19781116))  ;; another arbitrarily chosen birthday

(define rndf rand)

(define (random n) (floor (abs (* (rndf) n))))

(define (rndvec) (vector (rndf) (rndf) (rndf)))

(define (crndf)
  (* (- (rndf) 0.5) 2))

(define (crndvec)
  (vector (crndf) (crndf) (crndf)))

(define (srndvec)
  (let loop ((v (crndvec)))
    (if (> (vmag v) 1) ; todo: use non sqrt version
        (loop (crndvec))
        v)))

(define (hsrndvec)
  (let loop ((v (crndvec)))
    (let ((l (vmag v)))
      (if (or (> l 1) (eq? l 0))
          (loop (crndvec))
          (vdiv v l)))))

(define (grndf)
  (let loop ((x (crndf)) (y (crndf)))
    (let ((l (+ (* x x) (* y y))))
      (if (or (>= l 1) (eq? l 0))
          (loop (crndf) (crndf))
          (* (sqrt (/ (* -2 (log l)) l)) x)))))

(define (grndvec)
  (vector (grndf) (grndf) (grndf)))

(define (rndbary)
	(let*
		((a (- 1.0 (sqrt (rndf))))
		 (b (* (rndf) (- 1.0 a)))
		 (c (- 1.0 (+ a b))))
		(vector a b c)))

; return a line on the hemisphere
(define (rndhemi n)
  (let loop ((v (srndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (srndvec)))))

(define (hrndhemi n)
  (let loop ((v (hsrndvec)))
    (if (> (vdot n v) 0)
        v
        (loop (hsrndvec)))))

;------------------------------------------------------------

(define (get-line-from-screen x y)
  (let* ((ndcpos (vector (* (- (/ x (vx (get-screen-size))) 0.5) 3.5 0.8)
                         (* (- (- (/ y (vy (get-screen-size))) 0.5)) 4 0.8) 10))
         (scrpos2 (vtransform (vmul ndcpos 500) (get-camera-transform)))
         (scrpos (vtransform ndcpos (get-camera-transform))))
    (list scrpos scrpos2)))

(define (get-point-from-screen x y)
  (let ((line (get-line-from-screen x y)))
;    (display (car line)) (display "->") (display (cadr line))(newline)
    (let ((v (vlerp (car line) (cadr line) 0.008)))
;      (display v)(newline)
      v)))

(define (get-line-from-mouse)
  (get-line-from-screen (mouse-x) (mouse-y)))

(define (get-point-from-mouse)
  (get-point-from-screen (mouse-x) (mouse-y)))

(define (get-line-from-touch id)
  (let ((pos (get-pos-from-touch id)))
    (get-line-from-screen (car pos) (cadr pos))))

(define (get-point-from-touch id)
  (let ((pos (get-pos-from-touch id)))
    (get-point-from-screen (car pos) (cadr pos))))

; ------------------------------------------------------

(define (do-with-state a)
  (list 'begin '(push)
        (list 'let (list (list 'r (cons 'begin a)))
              '(pop) 'r)))

(define (do-with-primitive a)
  (list 'begin (list 'grab (car a))
        (list 'let
              (list (list 'r (cons 'begin (cdr a))))
              '(ungrab) 'r)))

(define (do-every-frame a)
  (list 'set! 'frame-thunk (append (list 'lambda '()) a)))

(define (diy-macro s)
  (cond
    ((null? s) s)
    ((list? s)
     (map
      (lambda (i)
        (if (and (list? i) (not (null? i)))
            (cond
              ((eq? (car i) 'with-state)
               (do-with-state (diy-macro (cdr i))))
              ((eq? (car i) 'with-primitive)
               (do-with-primitive (diy-macro (cdr i))))
              ((eq? (car i) 'every-frame)
               (do-every-frame (diy-macro (cdr i))))
              (else (diy-macro i)))
            (diy-macro i)))
      s))
    (else s)))

(define (load-pre-process-run filename)
  (let ((fi (open-input-file filename)))
    (let ((code (diy-macro (list 'begin (read fi)))))
      (close-input-port fi)
      (eval code))))

(define (pre-process-run code)
  (let ((code (diy-macro (append '(begin) code))))
;    (display code)(newline)
    (eval code)))

;;---------------------------------------------------------
;; jellyfish helpers

(define (jelly-compiled code)
  (define addr 0)
  (for-each
   (lambda (v)
     (pdata-set! "x" addr v)
     (set! addr (+ addr 1)))
   code))

(define (program-jelly speed prim-type code)
  (let ((c (compile-program speed prim-type 1 code)))
    ;;(disassemble c)
    (jelly-compiled c)))

(define (disassemble-compiled code)
  (let ((c (compile-program 50 'triangles 1 code)))
    (disassemble c))
  code)
