Jellyfish
=========

A minimalistic fluxus compatible programmable game engine and 3D
renderer for exprimenting with livecoding ARM devices such as Raspberry
Pi, OUYA and Android as well as legacy support for PlayStation2.

* Modified tinyscheme R5RS interpreter
* REPL for livecoding
* OSC REPL for remote livecoding
* Fixed point maths throughout
* An experimental vector processor and compiler for fast procedural
  rendering
* OpenGL ES backend for ARM/Android/Rasperry Pi/OUYA
* Linux target as a reference version (also running fixed point)
* Playstation 2 target (legacy) a custom hardware renderer running on
  vu1 path

Building
--------

You'll need scons and liblo-dev installed, the Linux version requires
GLUT.

### Linux ###

    scons

### Raspberry Pi ###

    scons TARGET=RPI

### Android/OUYA ###

As part of an APK:

    ndk-build

Running
-------

Inside the testing directory there is a python script for testing the
OSC mode, continuously sending test code to the jellyfish program -
which can be on another machine.

Jellyfish Lisp Language Reference
---------------------------------

### Example programs ###

The purpose of a Jellyfish Lisp program is to manipulate 3D objects in a
scene, move/rotate/scale or change vertex positions, lighting normals,
texture coords. It can also act on input from outside. It can do this
faster than in interpreted Scheme, particually on ARM devices as the VM
(written in C++) is very small and doesn't require any memory allocation
so no garbage collection is required. The VM also has access to more
data than a GPU shader, although tight coupling to GPU functionality (ie
running parts of the VM on GPU cores) is planned.

Jellyfish Lisp programs are compiled to bytecode executed by the VM -
there is one per object potentially running in parallel threads.  The
helper function (make-jelly-obj) takes a number of cycles to execute per
frame, an OpenGL primitive type and a program. It returns the primitive
id which can be operated on like any normal fluxus primitive.

Inside the jellyfish VM the 3D data exists in the same memory address
space as the program itself, starting at "positions-start"
address. Using (read) or (write!) you can access it using a value as an
address.

Here is a program that randomly moves vertex positions around:

    ;; normal fluxus code
    (with-primitive
     (make-jelly-obj 1000 prim-tristrip
    ;; jellyfish lisp starts here
      '(let ((vertex 0))
         (forever
          (set! vertex positions-start) ;; start at the first vertex number
          (loop (< vertex positions-end) ;; go to the last vertex
                (write! vertex (+ (read vertex) (rndvec))) ;; add a random amount
                (set! vertex (+ vertex 1))) ;; increment vertex number
         )))
    ;; jellyfish lisp ends here
    ;; normal fluxus code
     (pdata-map! (lambda (p) (srndvec)) "p")
     (pdata-map! (lambda (c) (rndvec)) "c"))


### Core forms ###

Reference for all the commands in jellyfish lisp.

#### let ####

    (let ((name value) (name value) ...) block)

Bind names to values. All values are internally vectors of length 3 in
jellyfish (as it's a vector processor), but you can specify them as
single numbers for convenience, which just get converted to
`(vector x 0 0)` where x is the number you specify.

*Tofix*: Let bindings are not scoped correctly, variables can be
referred to after scope is closed and conflict with function parameters
and other let variables of the same name. This is bad.

#### define ####

    (define name value)

Assign a name to a value, can be a function or a vector.

*Tofix*: same scoping problem same as let.

#### if ####

    (if pred true-expr false-expr)

An if expression - returns value of the executed expression, eg.:

    (define a (if (< (some-func) 10) 100 200))

Will assign a to be 100 or 200.

#### when ####

    (when pred do-this-block)

Same as if, without the false else block. Returns the value of the last
expression if true, zero otherwise.

#### cond ####

    (cond (pred block) (pred block) ... (else block))

Internally evaluates to a bunch of if's

#### loop ####

    (loop pred block)

Repeats until the predicate is false.

#### forever ####

    (forever block)

Repeats forever

#### do ####

    (do block)

Returns the value of the last expression.

#### lambda ####

    (lambda (args) block)

Creates a procedure, more usually:

    (define myproc
        (lambda (a b)
            (+ a b)))

#### + - ####

Vector addition and subtraction:

    (+ (vector 1 2 3) (vector 2 3 4))
    (+ 2 (vector 3 2 1))

Single numbers become (vector number 0 0) so the above lines evaluate to
`(vector 3 5 7)` and `(vector 5 2 1)` respectively. Same for subtraction.

#### * / ####

Multiplies and divides by the second parameter's x value, so:

    (* (vector 1 1 1) 2) => (vector 2 2 2)

and

    (/ (vector 1 1 1) 2) => (vector 0.5 0.5 0.5)

#### *v ####

Multiplies by each element, eg:

    (*v (vector 1 2 3) (vector 0.5 4 3)) => (vector 0.5 8 9)


#### cross dot ####

Cross product and dot product of two vectors

#### eq? ####

Per-element equality

#### > < ####

Compares x values of supplied vectors.
Tofix: why no <= or >=?

#### set! ####

    (set! name newvalue)

Mutates value of an existing variable.

#### write! ####



* write-add!
* swizzle
* rndvec
* trace
* read
* not
* mag
* magsq
* noise
* normalise
* abs
* floor
* sincos
* ignore
* round
* synth-create
* synth-connect
* synth-play
* play-now

### Low level instruction set ###

Soon...

jmp jmz jlt jgt ldl lda ldi sta sti
add sub mul div abs scs atn dot crs
sqr len dup drp cmp shf bld ret dbg
nrm mst mad msb swp rnd mull
jmr ldlv lensq noise lds sts mulv
synth-crt synth-con synth-ply flr
