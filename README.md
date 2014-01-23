Jellyfish
=========

Aims to be a fluxus compatible programmable game engine and 3D renderer
for livecoding small devices.

Incorporating:

* Modified tinyscheme R5RS interpreter
* REPL for livecoding
* OSC REPL for remote livecoding
* Fixed point maths throughout
* An experimental vector processor and compiler for fast procedural rendering
* OpenGL ES backend for ARM/Android/Rasperry Pi/OUYA
* Linux target: reference version (also running fixed point)
* Playstation 2 target (legacy)
  * Custom hardware renderer running on vu1 path

Building
--------

You'll need scons and liblo-dev installed, the Linux version requires GLUT.

### Linux ###

    scons

### Raspberry Pi ###

    scons TARGET=RPI

### Android/OUYA ###

As part of an APK:

    ndk-build

Jellyfish Lisp Language Reference
---------------------------------

### Example programs ###


Randomly move vertex positions

    (with-primitive
     (make-jelly-obj 1000
    ;; jellyfish lisp starts here
      '(let ((vertex positions-start))
         (forever
          (set! vertex positions-start)
          (loop (< vertex positions-end)
                (write! vertex (+ (read vertex) (rndvec)))
                (set! vertex (+ vertex 1)))
         )))
    ;; jellyfish lisp ends here
     (pdata-map! (lambda (p) (srndvec)) "p")
     (pdata-map! (lambda (c) (rndvec)) "c"))


### Core forms ###

TDB

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

TDB

jmp jmz jlt jgt ldl lda ldi sta sti
add sub mul div abs scs atn dot crs
sqr len dup drp cmp shf bld ret dbg
nrm add.x add.y add.z swp rnd mull
jmr ldlv lensq noise lds sts mulv
synth-crt synth-con synth-ply flr
