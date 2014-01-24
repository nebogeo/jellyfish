Jellyfish
=========

A minimalistic fluxus compatible programmable game engine and 3D
renderer for exprimenting with livecoding ARM devices such as Raspberry
Pi, OUYA and Android as well as legacy support for PlayStation2.

Incorporating:

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

The purpose of a jellyfish lisp program is to manipulate 3D objects,
move/rotate/scale or change it's vertex positions, lighting normals,
texture coords. It can also act on input from outside. It can do this
faster than in Scheme (particually on ARM devices) as it doesn't require
any memory allocation, and it has access to more data than a GPU shader.

Jellyfish Lisp programs are compiled to bytecode executed by the
Jellyfish VM - there is one per object running parallel.  The helper
function (make-jelly-obj) takes a number of cycles to execute per frame,
an OpenGL primitive type and a program. It returns the primitive id
which can be operated on like any normal fluxus primitive.

Inside the jellyfish VM the 3D data exists in the same memory address
space as the program, starting at "positions-start" address. You can
(read) data or (write!) to it using a value as an address.

Here is a program that randomly moves vertex positions around:

    ;; normal fluxus code
    (with-primitive
     (make-jelly-obj 1000 prim-tristrip
    ;; jellyfish lisp starts here
      '(let ((vertex positions-start))
         (forever
          (set! vertex positions-start)
          (loop (< vertex positions-end)
                (write! vertex (+ (read vertex) (rndvec)))
                (set! vertex (+ vertex 1)))
         )))
    ;; jellyfish lisp ends here
    ;; normal fluxus code
     (pdata-map! (lambda (p) (srndvec)) "p")
     (pdata-map! (lambda (c) (rndvec)) "c"))


### Core forms ###

Willdo...

* let
* define
* cond
* loop
* forever
* do
* lambda
* +
* -
* *
* /
* *v
* cross
* dot
* eq?
* >
* <
* set!
* write!
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
nrm add.x add.y add.z swp rnd mull
jmr ldlv lensq noise lds sts mulv
synth-crt synth-con synth-ply flr
