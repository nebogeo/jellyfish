;;#lang racket
;; vectorlisp: a strange language for procedural rendering

(define debug #f)
(define prim-size 4096)

(define jfsh-op-nop 0) (define jfsh-op-jmp 1) (define jfsh-op-jmz 2) (define jfsh-op-jlt 3) (define jfsh-op-jgt 4)
(define jfsh-op-ldl 5) (define jfsh-op-lda 6) (define jfsh-op-ldi 7) (define jfsh-op-sta 8) (define jfsh-op-sti 9)
(define jfsh-op-add 10) (define jfsh-op-sub 11) (define jfsh-op-mul 12) (define jfsh-op-div 13) (define jfsh-op-abs 14)
(define jfsh-op-sincos 15) (define jfsh-op-atn 16) (define jfsh-op-dot 17) (define jfsh-op-crs 18) (define jfsh-op-sqr 19)
(define jfsh-op-len 20) (define jfsh-op-dup 21) (define jfsh-op-drp 22) (define jfsh-op-cmp 23) (define jfsh-op-shf 24)
(define jfsh-op-bld 25) (define jfsh-op-ret 26) (define jfsh-op-dbg 27) (define jfsh-op-nrm 28)
(define jfsh-op-mst 29) (define jfsh-op-mad 30) (define jfsh-op-msb 31) (define jfsh-op-end-check 999)
(define jfsh-op-swp 32) (define jfsh-op-rnd 33) (define jfsh-op-mull 34) (define jfsh-op-jmr 35) (define jfsh-op-ldlv 36)
(define jfsh-op-lensq 37) (define jfsh-op-noise 38) (define jfsh-op-lds 39) (define jfsh-op-sts 40) (define jfsh-op-mulv 41)
(define jfsh-op-synth-crt 42) (define jfsh-op-synth-con 43) (define jfsh-op-synth-ply 44) (define jfsh-op-flr 45)
(define jfsh-op-mod 46) (define jfsh-op-mulm 47)

(define instr
  '(nop jmp jmz jlt jgt ldl lda ldi sta sti
        add sub mul div abs scs atn dot crs
        sqr len dup drp cmp shf bld ret dbg
        nrm mst mad msb swp rnd mull
        jmr ldlv lensq noise lds sts mulv
        synth-crt synth-con synth-ply flr mod))

(define prim-triangles 0)
(define prim-tristrip 1)
(define prim-points 2)
(define prim-lines 3)
(define prim-linestrip 4)

;; registers
(define reg-control 0) ;; pc, cycles, stack
(define reg-graphics 1) ;; size, primtype, hints
(define reg-tx-translate 2)
(define reg-tx-rotatea 3)
(define reg-tx-rotateb 4)
(define reg-tx-rotatec 5)
(define reg-fling 6)
(define reg-code-start 7)

(define emit list)

;; variables are just an address lookup table
(define variables '())

(define (make-variable! name)
  (when (not (memq name variables))
        (set! variables (append variables (list name)))))

(define (variable-address name)
  (define (_ l c)
    (cond
     ((null? l) (msg "cant find variable " name) #f)
     ((equal? name (car l)) c)
     (else (_ (cdr l) (+ c 1)))))
  ;; code-start is address after registers
  (_ variables reg-code-start))

;; variables are just an address lookup table
(define function-code '())

;; returns address relative to start of function block
(define (make-function! code)
  (let ((addr (length function-code)))
    (set! function-code (append function-code code))
    addr))

;; segments are data areas, positions, normals, colours etc
(define (memseg n) (* prim-size n))

(define (get-segment name)
  (cond
   ((eq? name 'code-start) (memseg 0))
   ((eq? name 'code-end) (- (memseg 1) 1))
   ((eq? name 'positions-start) (memseg 1))
   ((eq? name 'positions-end) (- (memseg 2) 1))
   ((eq? name 'normals-start) (memseg 2))
   ((eq? name 'normals-end) (- (memseg 3) 1))
   ((eq? name 'colours-start) (memseg 3))
   ((eq? name 'colours-end) (- (memseg 4) 1))
   ((eq? name 'texture-start) (memseg 4))
   ((eq? name 'texture-end) (- (memseg 5) 1))
   (else #f)))

;; when we find a symbol, look it up
(define (emit-constant-or-variable name)
  (let ((seg (get-segment name)))
    (cond
     ;; if a memory segment constant is found
     (seg (emit (vector jfsh-op-ldl seg 0)))
     ;; other constants
     ((eq? name 'reg-control) (emit (vector jfsh-op-ldl reg-control 0)))
     ((eq? name 'reg-graphics) (emit (vector jfsh-op-ldl reg-graphics 0)))
     ((eq? name 'reg-tx-translate) (emit (vector jfsh-op-ldl reg-tx-translate 0)))
     ((eq? name 'reg-tx-rotatea) (emit (vector jfsh-op-ldl reg-tx-rotatea 0)))
     ((eq? name 'reg-tx-rotateb) (emit (vector jfsh-op-ldl reg-tx-rotateb 0)))
     ((eq? name 'reg-tx-rotatec) (emit (vector jfsh-op-ldl reg-tx-rotatec 0)))
     ((eq? name 'reg-fling) (emit (vector jfsh-op-ldl reg-fling 0)))
     ((eq? name 'prim-size) (emit (vector jfsh-op-ldl prim-size 0)))
     ;; load variable
     (else
      (emit (vector jfsh-op-lda (variable-address name) 0))))))

;; create empty space for variable data
(define (variables->data)
  (map
   (lambda (i)
     (vector 0 0 0))
   variables))

;; is this an immediate value
(define (immediate? x)
  (or (number? x) (symbol? x) (eq? (car x) 'vector)))

;; push whatever this immediate value is onto the stack
(define (emit-push x)
  (cond
   ((number? x) (emit (vector jfsh-op-ldl x 0)))
   ((vector? x) (emit (vector jfsh-op-ldlv 0 0) x))
   ((symbol? x) (emit-constant-or-variable x))
   ((list? x) (emit (vector jfsh-op-ldlv 0 0) (list->vector (cdr x))))
   (else
    (error "can't push" x))))

;; is this a primitive call?
(define (primcall? x)
  (and (list? x) (not (null? x)) (symbol? (car x))))

;; append a bunch of expressions
(define (emit-expr-list l)
  (cond
    ((null? l) '())
    (else
     (append (emit-expr (car l))
             (if (null? (cdr l)) '()
                 (append
                  ;; insert drops to ignore returned data
                  ;; from expressions in the list we're not
                  ;; using (must have side effects)
                  ;; - keep last push
                  (emit (vector jfsh-op-drp 0 0))
                  (emit-expr-list (cdr l))))))))


;; append a bunch of expressions, don't drop
;; as we want to build the stack (for fn call)
(define (emit-expr-list-maintain-stack l)
  (cond
    ((null? l) '())
    (else
     (append (emit-expr (car l))
             (if (null? (cdr l)) '()
                 (emit-expr-list-maintain-stack (cdr l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitive function calls follow

(define (emit-set! x)
  (append
   (emit-expr (caddr x))
   (emit
    (vector jfsh-op-sta (variable-address (cadr x)) 0))
   (emit (vector jfsh-op-ldl 0 0))))

;; (write! start-addr value value value ...)
(define (emit-write! x)
  (append
   ;; stick everything on the stack
   (emit-expr-list-maintain-stack (reverse (cdr x)))
   (emit (vector jfsh-op-mst (length (cddr x)) 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-write-add! x)
  (append
   ;; stick everything on the stack
   (emit-expr-list-maintain-stack (reverse (cdr x)))
   (emit (vector jfsh-op-mad (length (cddr x)) 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-write-sub! x)
  (append
   ;; stick everything on the stack
   (emit-expr-list-maintain-stack (reverse (cdr x)))
   (emit (vector jfsh-op-msb (length (cddr x)) 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-read x)
  (append
   (emit-expr (cadr x)) ;; address
   (emit (vector jfsh-op-lds 0 0))))

(define (emit-addr x)
  (emit (vector jfsh-op-ldl (variable-address (cadr x)) 0)))

;; (if pred true-expr false-expr)
(define (emit-if x)
  (let ((tblock (emit-expr (caddr x))) ;; compile true expression to a block
        (fblock (emit-expr (cadddr x)))) ;; compile false expression to block
    (append
     (emit-expr (cadr x)) ;; predicate - returns true or false
     (emit (vector jfsh-op-jmz (+ (length tblock) 2) 0)) ;; if false skip true block
     tblock
     (emit (vector jfsh-op-jmr (+ (length fblock) 1) 0)) ;; skip false block
     fblock)))

;; (when pred true-block)
(define (emit-when x)
  (let ((block (emit-expr-list (cddr x)))) ;; compile the list of expressions
    (append
     (emit-expr (cadr x)) ;; predicate - returns true or false
     (emit (vector jfsh-op-jmz (+ (length block) 2) 0)) ;; skip the block
     block
     (emit (vector jfsh-op-jmr 2 0))    ;; return result of the block (skip next instr)
     (emit (vector jfsh-op-ldl 0 0))))) ;; return 0 if we didn't run the block

(define (emit-fncall x addr)
  (let ((args (emit-expr-list-maintain-stack (cdr x))))
    (append
     ;; offset from here -> stitch up in second pass
     (emit (list 'add-abs-loc 'this 1
                 (vector jfsh-op-ldl (+ (length args) 3) 0)))
     args ;; push arguments to stack
     (emit (vector jfsh-op-lda addr 0)) ;; fn ptr is in data mem
     (emit (vector jfsh-op-ret 0 0))))) ;; jump to fn

;; lambda args body
(define (emit-lambda x)
  (let* ((body
          (append
           (map
            (lambda (arg)
              ;; for moment use global pile for arguments :O
              (make-variable! arg)
              (vector jfsh-op-sta (variable-address arg) 0))
            (reverse (cadr x)))
           ;; now args are loaded, do body
           (emit-expr-list (cddr x))
           ;; swap ret ptr to top
           (emit (vector jfsh-op-swp 0 0))
           (emit (vector jfsh-op-ret 0 0))))
         (loc (make-function! body)))
    (append
     (if debug (emit (list "function code...")) '())
     (emit
      ;; offset from function code -> stitch up in linking pass
       (list 'add-abs-loc 'function-code 1
            (vector jfsh-op-ldl loc 0))))))

(define (emit-define x)
  (make-variable! (cadr x))
  (append
   (emit-expr (caddr x))
   (emit (vector jfsh-op-sta (variable-address (cadr x)) 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-let-part x)
  (make-variable! (car x))
  (append
   (emit-expr (cadr x))
   (emit (vector jfsh-op-sta (variable-address (car x)) 0))))

(define (emit-let x)
  (define (_ l)
    (cond
     ((null? l) '())
     (else (append (emit-let-part (car l))
                   (_ (cdr l))))))
  (append
   (_ (cadr x))
   (emit-expr-list (cdr (cdr x)))))

(define (emit-trace x)
  (append
   (emit-expr (cadr x))
   (emit (vector jfsh-op-dbg 0 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-not x)
  (append
   (emit-expr (cadr x))
   (emit (vector jfsh-op-jmz 3 0))
   (emit (vector jfsh-op-ldl 0 0))
   (emit (vector jfsh-op-jmr 2 0))
   (emit (vector jfsh-op-ldl 1 0))))

(define (emit-and x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jfsh-op-jmz 4 0)) ;; if first is zero skip to load 0
   (emit (vector jfsh-op-jmz 3 0)) ;; second zero load 0
   (emit (vector jfsh-op-ldl 1 0)) ;; load 1
   (emit (vector jfsh-op-jmr 2 0)) ;; skip
   (emit (vector jfsh-op-ldl 0 0)))) ;; load 0

(define (emit-or x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jfsh-op-jmz 4 0)) ;; zero, check next
   (emit (vector jfsh-op-drp 0 0)) ;; otherwise drop other
   (emit (vector jfsh-op-ldl 1 0)) ;; load 1
   (emit (vector jfsh-op-jmr 5 0)) ;; exit
   (emit (vector jfsh-op-jmz 3 0)) ;; if also zero jump to load 0
   (emit (vector jfsh-op-ldl 1 0)) ;; load 1
   (emit (vector jfsh-op-jmr 2 0)) ;; skip
   (emit (vector jfsh-op-ldl 0 0)))) ;; load 0

;(loop pred block)
(define (emit-loop x)
  (let ((block
         (append
          (emit-expr-list (cdr (cdr x)))
          (emit (vector jfsh-op-drp 0 0))
          (emit-expr (cadr x)))))
    (append
     block
     (emit (vector jfsh-op-jmz 2 0))
     (emit (vector jfsh-op-jmr (- (+ (length block) 1)) 0))
     (emit (vector jfsh-op-ldl 0 0))
     )))

(define (unary-procedure proc x)
  (append
   (emit-expr (cadr x))
   (emit (vector proc 0 0))))

(define (binary-procedure proc x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector proc 0 0))))

(define (trinary-procedure proc x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit-expr (cadddr x))
   (emit (vector proc 0 0))))

(define (emit-eq? x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jfsh-op-sub 0 0))
   (emit (vector jfsh-op-jmz 3 0))
   (emit (vector jfsh-op-ldl 0 0))
   (emit (vector jfsh-op-jmr 2 0))
   (emit (vector jfsh-op-ldl 1 0))))

(define (emit-< x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jfsh-op-jlt 3 0))
   (emit (vector jfsh-op-ldl 1 0))
   (emit (vector jfsh-op-jmr 2 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-> x)
  (append
   (emit-expr (cadr x))
   (emit-expr (caddr x))
   (emit (vector jfsh-op-jgt 3 0))
   (emit (vector jfsh-op-ldl 1 0))
   (emit (vector jfsh-op-jmr 2 0))
   (emit (vector jfsh-op-ldl 0 0))))

(define (emit-swizzle x)
  (append
   (emit-expr (caddr x))
   (emit (vector jfsh-op-ldlv 0 0))
   (cond
    ((eq? (cadr x) 'xxx) (emit (vector 0 0 0)))
    ((eq? (cadr x) 'xxy) (emit (vector 0 0 1)))
    ((eq? (cadr x) 'xxz) (emit (vector 0 0 2)))
    ((eq? (cadr x) 'xyx) (emit (vector 0 1 0)))
    ((eq? (cadr x) 'xyy) (emit (vector 0 1 1)))
    ((eq? (cadr x) 'xyz) (emit (vector 0 1 2)))
    ((eq? (cadr x) 'xzx) (emit (vector 0 2 0)))
    ((eq? (cadr x) 'xzy) (emit (vector 0 2 1)))
    ((eq? (cadr x) 'xzz) (emit (vector 0 2 2)))
    ((eq? (cadr x) 'yxx) (emit (vector 1 0 0)))
    ((eq? (cadr x) 'yxy) (emit (vector 1 0 1)))
    ((eq? (cadr x) 'yxz) (emit (vector 1 0 2)))
    ((eq? (cadr x) 'yyx) (emit (vector 1 1 0)))
    ((eq? (cadr x) 'yyy) (emit (vector 1 1 1)))
    ((eq? (cadr x) 'yyz) (emit (vector 1 1 2)))
    ((eq? (cadr x) 'yzx) (emit (vector 1 2 0)))
    ((eq? (cadr x) 'yzy) (emit (vector 1 2 1)))
    ((eq? (cadr x) 'yzz) (emit (vector 1 2 2)))
    ((eq? (cadr x) 'zxx) (emit (vector 2 0 0)))
    ((eq? (cadr x) 'zxy) (emit (vector 2 0 1)))
    ((eq? (cadr x) 'zxz) (emit (vector 2 0 2)))
    ((eq? (cadr x) 'zyx) (emit (vector 2 1 0)))
    ((eq? (cadr x) 'zyy) (emit (vector 2 1 1)))
    ((eq? (cadr x) 'zyz) (emit (vector 2 1 2)))
    ((eq? (cadr x) 'zzx) (emit (vector 2 2 0)))
    ((eq? (cadr x) 'zzy) (emit (vector 2 2 1)))
    ((eq? (cadr x) 'zzz) (emit (vector 2 2 2))))
   (emit (vector jfsh-op-shf 0 0))))

(define (emit-procedure x)
  (cond
    ((eq? (car x) '+) (binary-procedure jfsh-op-add x))
    ((eq? (car x) '-) (binary-procedure jfsh-op-sub x))
    ((eq? (car x) '*) (binary-procedure jfsh-op-mul x))
    ((eq? (car x) '/) (binary-procedure jfsh-op-div x))
    ((eq? (car x) '*v) (binary-procedure jfsh-op-mulv x))
    ((eq? (car x) '*m) (trinary-procedure jfsh-op-mulm x))
    ((eq? (car x) 'cross) (binary-procedure jfsh-op-crs x))
    ((eq? (car x) 'dot) (binary-procedure jfsh-op-dot x))
    ((eq? (car x) 'modulo) (binary-procedure jfsh-op-mod x))
    ((eq? (car x) 'eq?) (emit-eq? x))
    ((eq? (car x) '>) (emit-> x))
    ((eq? (car x) '<) (emit-< x))
    ((eq? (car x) 'set!) (emit-set! x))
    ((eq? (car x) 'write!) (emit-write! x))
    ((eq? (car x) 'write-add!) (emit-write-add! x))
    ((eq? (car x) 'write-sub!) (emit-write-sub! x))
    ((eq? (car x) 'swizzle) (emit-swizzle x))
    ((eq? (car x) 'lambda) (emit-lambda x))
    ((eq? (car x) 'rndvec) (emit (vector jfsh-op-rnd 0 0)))
    ((eq? (car x) 'trace) (emit-trace x))
    ((eq? (car x) 'read) (emit-read x))
    ((eq? (car x) 'addr) (emit-addr x))
    ((eq? (car x) 'not) (emit-not x))
    ((eq? (car x) 'and) (emit-and x))
    ((eq? (car x) 'or) (emit-or x))
    ((eq? (car x) 'mag) (unary-procedure jfsh-op-len x))
    ((eq? (car x) 'magsq) (unary-procedure jfsh-op-lensq x))
    ((eq? (car x) 'noise) (unary-procedure jfsh-op-noise x))
    ((eq? (car x) 'normalise) (unary-procedure jfsh-op-nrm x))
    ((eq? (car x) 'abs) (unary-procedure jfsh-op-abs x))
    ((eq? (car x) 'floor) (unary-procedure jfsh-op-flr x))
    ((eq? (car x) 'sincos) (unary-procedure jfsh-op-sincos x))
    ((eq? (car x) 'ignore) (unary-procedure jfsh-op-drp x))
    ((eq? (car x) 'round)
     (append
      (emit-expr (cadr x))
      (emit (vector jfsh-op-ldlv 0 0))
      (emit (vector 0.5 0.5 0.5))
      (emit (vector jfsh-op-add 0 0))
      (emit (vector jfsh-op-flr 0 0))))
    (else
     (let ((addr (variable-address (car x))))
       (if addr
           (emit-fncall x addr)
           (begin
             (msg "unknown function: " x) '()))))))

(define (emit-expr x)
  (cond
   ((immediate? x) (emit-push x))
   ((primcall? x)
    (append
     (if debug
         (list (string-append "starting " (symbol->string (car x))))
         '())
     (cond
      ((eq? (car x) 'let) (emit-let x))
      ((eq? (car x) 'define) (emit-define x))
      ((eq? (car x) 'if) (emit-if x))
      ((eq? (car x) 'when) (emit-when x))
      ((eq? (car x) 'loop) (emit-loop x))
      ((eq? (car x) 'do) (emit-expr-list (cdr x)))
      (else (emit-procedure x)))
     (if debug
         (list (string-append "ending " (symbol->string (car x))))
         '())
     ))
   (else
    (msg "don't understand " x) '())))

(define (header code-start cycles prim hints)
  (list
   (vector code-start cycles 0) ;; control (pc, cycles, stack)
   (vector prim-size prim hints) ;; graphics
   (vector 0 0 0) ;; translate
   (vector 1 0 0) ;; rota
   (vector 0 1 0) ;; rotb
   (vector 0 0 1) ;; rotc
   (vector 0 0 0))) ;; fling

(define (link-add code addr)
  (let ((v (list-ref code 3)))
    (vector-set! v (list-ref code 2)
                 (+ (vector-ref v (list-ref code 2))
                    addr))
    v))

(define (link-dangling code function-start pc)
  (cond
   ((eq? (car code) 'add-abs-loc)
    (cond
     ((eq? (cadr code) 'this)
      (link-add code pc))
     ((eq? (cadr code) 'function-code)
      (link-add code function-start))
     (else
      (msg "link offset unhandled:" code))))
   (else
    (msg "link type unhandled:" code))))

(define (link-program code function-start pc)
  (cond
   ((null? code) '())
   ((list? (car code))
    (cons
     (link-dangling (car code) function-start pc)
     (link-program (cdr code) function-start (+ pc 1))))
   (else
    (cons
     (car code)
     (link-program (cdr code) function-start (+ pc 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (preprocess-cond-to-if x)
  (define (_ l)
    (cond
      ((null? l) 0)
      ((eq? (pre-process (caar l)) 'else) (cons 'do (pre-process (cdr (car l)))))
      (else (list 'if (pre-process (caar l)) (cons 'do (pre-process (cdr (car l))))
                  (_ (cdr l))))))
  (_ (cdr x)))



(define (preprocess-map x)
  (pre-process x)
  )

;; basically diy-macro from the main tinyscheme stuff
(define (pre-process s)
  (cond
    ((null? s) s)
    ((list? s)
     (map
      (lambda (i)
        (if (and (list? i) (not (null? i)))
            ;; dispatch to macro processors
            (cond
             ((eq? (car i) 'forever) (append (list 'loop 1) (pre-process (cdr i))))
             ((eq? (car i) '++!)
              (let ((v (pre-process (cadr i))))
                (dbg (list 'set! v (list '+ v 1)))))
             ((eq? (car i) '--!)
              (let ((v (pre-process (cadr i))))
                (list 'set! v (list '- v 1))))
             ((eq? (car i) 'cond) (preprocess-cond-to-if i))
             ((eq? (car i) 'map!) (preprocess-map i))
             (else (pre-process i)))
            (pre-process i)))
      s))
    (else s)))

(define (compile-program cycles prim hints x)
  (set! variables '())
  (set! function-code '())
  (let* ((code (emit-expr (pre-process x)))
         (data (variables->data))
         (function-start (+ reg-code-start (length data)))
         (out
          (link-program
           (append
            (header (+ reg-code-start (length data)
                       (length function-code))
                    cycles prim hints)
            data
            function-code
            code)
           function-start 0)))
    ;;(msg "code is as follows")
    ;;(disassemble out)
    (msg "compiled length is" (length out))
    out))

(define (disassemble code)
  (define ln 0)
  (for-each
   (lambda (i)
     (display ln)(display ": ")
     (cond
      ((vector? i)
       (msg i)
       (if (and (>= (vector-ref i 0) 0)
                (< (vector-ref i 0) (length instr)))
           (begin
             (display (list-ref instr (inexact->exact (round (vector-ref i 0)))))
             (display (string-append "(" (number->string (vector-ref i 0)) ")")))
           (display (vector-ref i 0)))
       (display " ")(display (vector-ref i 1))
       (display " ")(display (vector-ref i 2))(newline))
      ((symbol? i) (display (string-append ";; " (symbol->string i)))(newline))
      (else (display ";; ")(display i)(newline)))
     (set! ln (+ ln 1)))
   code))
