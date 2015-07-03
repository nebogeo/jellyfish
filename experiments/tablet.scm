(synth-init 20 44100)

(define (flip a)
  (if (equal? a "s") "z" "s"))

(define (card angle a b d c) (list angle a b d c "f" "f"))
(define (card-angle c) (list-ref c 0))
(define (card-a c) (list-ref c 1))
(define (card-b c) (list-ref c 2))
(define (card-d c) (list-ref c 3))
(define (card-c c) (list-ref c 4))
(define (card-memory c) (list-ref c 5))
(define (card-previous-memory c) (list-ref c 6))
(define (card-print c)
  (display (card-a c))(display " ")(display (card-b c))(newline)
  (display (card-d c))(display " ")(display (card-c c))(newline))

(define (card-forward c)
  (list
   (card-angle c)
   (card-d c) (card-a c)
   (card-c c) (card-b c)
   "f"
   (card-memory c)))

(define (card-back c)
  (list
   (card-angle c)
   (card-b c) (card-c c)
   (card-a c) (card-d c)
   "b"
   (card-memory c)))

(define (card-flip c)
  (list
   (flip (card-angle c))
   (card-b c) (card-a c)
   (card-c c) (card-d c)
   (card-memory c)
   (card-previous-memory c)))

(define (card-weave c)
  (if (equal? (card-memory c) "f")
      (list (card-b c) (card-d c))
      (list (card-a c) (card-c c))))

(define (card-loom cards) (list cards))
(define (card-loom-cards c) (list-ref c 0))

(define (card-loom-all-forward c)
  (card-loom
   (map
    (lambda (card)
      (card-forward card))
    (card-loom-cards c))))

(define (card-loom-all-back c)
  (card-loom
   (map
    (lambda (card)
      (card-back card))
    (card-loom-cards c))))

(define (in-list? l c)
  (cond
    ((null? l) #f)
    ((equal? (car l) c) #t)
    (else (in-list? (cdr l) c))))

(define (card-loom-flip loom c)
  (define pos (- 0 1))
  (card-loom
   (map
    (lambda (card)
      (set! pos (+ pos 1))
      (if (in-list? c pos)
          (card-flip card) card))
    (card-loom-cards loom))))

(define (card-loom-ind-forward loom c)
  (define pos (- 0 1))
  (card-loom
   (map
    (lambda (card)
      (set! pos (+ pos 1))
      (if (in-list? c pos)
          (card-forward card) card))
    (card-loom-cards loom))))

(define (card-loom-ind-back loom c)
  (define pos (- 0 1))
  (card-loom
   (map
    (lambda (card)
      (set! pos (+ pos 1))
      (if (in-list? c pos)
          (card-back card) card))
    (card-loom-cards loom))))

(define pos 10)

(define (card-to-direction card)
  (if (eq? (card-memory card) (card-previous-memory card))
      (if (eq? (card-memory card) "b")
          (if (eq? (card-angle card) "z") "s" "z")
          (if (eq? (card-angle card) "z") "z" "s"))
      "i"))

(define (card-loom-weave-top loom)
  (define canvas (document.getElementById "canvas"))
  (define ctx (canvas.getContext "2d"))
  (set! pos (+ pos 14))


  (index-for-each
   (lambda (i card)
     (let ((x (+ (* i 14) 0)))
       ;(ctx.strokeRect x pos 12 12)

       (ctx.beginPath)
       (ctx.arc (+ x 2) (+ pos 2) 2 (* Math.PI 2) #f)
       (set! ctx.fillStyle (if (eq? (card-a card) "#") "#000" "#fff"))
       (ctx.fill)
       (ctx.closePath)

       (ctx.beginPath)
       (ctx.arc (+ x 8) (+ pos 2) 2 (* Math.PI 2) #f)
       (set! ctx.fillStyle (if (eq? (card-b card) "#") "#000" "#fff"))
       (ctx.fill)
       (ctx.closePath)

       (ctx.beginPath)
       (ctx.arc (+ x 8) (+ pos 8) 2 (* Math.PI 2) #f)
       (set! ctx.fillStyle (if (eq? (card-c card) "#") "#000" "#fff"))
       (ctx.fill)
       (ctx.closePath)

       (ctx.beginPath)
       (ctx.arc (+ x 2) (+ pos 8) 2 (* Math.PI 2) #f)
       (set! ctx.fillStyle (if (eq? (card-d card) "#") "#000" "#fff"))
       (ctx.fill)
       (ctx.closePath)



       ))
   (card-loom-cards loom))


  (index-for-each
   (lambda (i card)
     (let ((direction (card-to-direction card))
           (colour (if (eq? (car (card-weave card)) "#") "black" "white")))
       (ctx.drawImage (find-image (string-append "tabwarp-" direction "-" colour ".png"))
                      (+ (* i 7) 280) pos)))
   (card-loom-cards loom))
  (index-for-each
   (lambda (i card)
     (let ((direction (card-to-direction card))
           (colour (if (eq? (cadr (card-weave card)) "#") "black" "white")))
       (ctx.drawImage (find-image (string-append "tabwarp-" direction "-" colour ".png"))
                      (+ (* i 7) 450) pos)))
   (card-loom-cards loom))
  loom)

(define time (ntp-time-add (ntp-time) 1))

(define (card-loom-weave-top-ascii loom)
  (let ((weave
         (foldl
          (lambda (card r)
            (string-append r (car (card-weave card))
                           ;;(if (equal? (card-angle card) "s") "\\" "/")
                           ))
          ""
          (card-loom-cards loom))))

    (play time (dbg (weave-1 (dbg (string->list weave)))) 0)
    (set! time (ntp-time-add time 0.3))
    (msg weave) loom))


(define (card-loom-multi loom fn c)
  (cond
    ((zero? c) loom)
    (else
     (card-loom-multi
      (card-loom-weave-top-ascii (fn loom)) fn (- c 1)))))

(define (card-loom-loop loom c commands)
  (cond
    ((zero? c) loom)
    (else
     (card-loom-loop
      (foldl
       (lambda (command loom)
         (card-loom-interpret loom command))
       loom commands)
      (- c 1) commands))))

(define (card-loom-interpret loom command)
  (cond
    ((list? command)
     (cond
      ((equal? (car command) "weave_forward")
       (card-loom-multi loom card-loom-all-forward (cadr command)))
      ((equal? (car command) "weave_back")
       (card-loom-multi loom card-loom-all-back (cadr command)))
      ((equal? (car command) "twist")
       (card-loom-flip loom (cdr command)))
      ((equal? (car command) "rotate_forward")
       (card-loom-ind-forward loom (cdr command)))
      ((equal? (car command) "rotate_back")
       (card-loom-ind-back loom (cdr command)))
      ((equal? (car command) "repeat")
       (card-loom-loop loom (cadr command) (cddr command)))
      (else
       (display "unknown function ")
       (display (car command))(newline) loom)))
    (else (display "unknown command ")(display command)(newline) loom)))



(define (card-loom-run loom code)
  (cond
    ((null? code) loom)
    (else
     (card-loom-run
      (card-loom-interpret loom (car code))
      (cdr code)))))


(define (assert q)
  (when (not q)
        (display "test failed...")
        (newline)))

(define (test)
  (assert (equal? (card-forward
                   (card "s"
                         "#" "."
                         "." "."))
                  (list "s"
                        "." "#"
                        "." "." "f" "f")))
  (assert (equal? (card-forward
                   (card "s"
                         "#" "."
                         "." "#"))
                  (list "s"
                        "." "#"
                        "#" "." "f" "f")))
  (assert (equal? (card-back
                   (card "z"
                         "#" "#"
                         "." "."))
                  (list "z"
                        "#" "."
                        "#" "." "b" "f")))
  (assert (equal? (card-back
                   (card-back
                    (card "z"
                          "#" "."
                          "#" ".")))
                  (list "z"
                        "." "#"
                        "." "#" "b" "b")))
  (assert (equal? (card-forward
                   (card-forward
                    (card-forward
                     (card-forward
                      (card "s"
                            "1" "4"
                            "2" "3")))))
                  (list "s"
                        "1" "4"
                        "2" "3" "f" "f")))

  (assert (equal? (card-flip (card "s"
                                   "1" "2"
                                   "3" "4"))
                  (card "z"
                        "2" "1"
                        "4" "3")))

  (assert (equal? (card-weave
                   (card "z"
                         "1" "2"
                         "4" "3")) (list "2" "4")))

  (assert (equal? (card-weave
                   (card-back
                    (card "z"
                          "1" "2"
                          "4" "3"))) (list "2" "4")))

  (assert (equal? (card-weave
                   (card-back
                    (card-back
                     (card "z"
                           "1" "2"
                           "4" "3")))) (list "3" "1")))
  (display "test passed") (newline)
  )

(test)

(define loom
  (card-loom
   (list
    (card "z"
          "a" "b"
          "d" "c")
    (card "z"
          "a" "b"
          "d" "c")
    (card "z"
          "a" "b"
          "d" "c")
    (card "z"
          "a" "b"
          "d" "c")
    (card "z"
          "a" "b"
          "d" "c")
    (card "z"
          "a" "b"
          "d" "c")
    )))


(define (weave-6 w)
  (cond
   ((equal? (car w) #\a) 10)
   ((equal? (car w) #\b) 20)
   ((equal? (car w) #\c) 12)
   ((equal? (car w) #\d) 14)))

(define (weave-5 w)
  (cond
   ((equal? (car w) #\a)
    (note (+ 2 (weave-6 (cdr w)))))
   ((equal? (car w) #\b)
    (note (+ 4 (weave-6 (cdr w)))))
   ((equal? (car w) #\c)
    (note (+ 3 (weave-6 (cdr w)))))
   ((equal? (car w) #\d)
    (note (+ 1 (weave-6 (cdr w)))))))

(define (weave-4 w)
  (cond
   ((equal? (car w) #\a)
    (sine (weave-5 (cdr w))))
   ((equal? (car w) #\b)
    (squ (weave-5 (cdr w))))
   ((equal? (car w) #\c)
    (tri (weave-5 (cdr w))))
   ((equal? (car w) #\d)
    (saw (weave-5 (cdr w))))))

(define (weave-3 w)
  (mul
   (cond
    ((equal? (car w) #\a)
     (adsr 0 0.1 0 0))
    ((equal? (car w) #\b)
     (adsr 0 0.1 0.1 1))
    ((equal? (car w) #\c)
     (adsr 0.2 0.1 0 0))
    ((equal? (car w) #\d)
     (adsr 0 0.2 0 0)))
   (weave-4 (cdr w))))


(define (weave-2 w)
  (cond
   ((equal? (car w) #\a)
    0.1)
   ((equal? (car w) #\b)
    0.2)
   ((equal? (car w) #\c)
    0.5)
   ((equal? (car w) #\d)
    0.9)))

(define (weave-1 w)
  (cond
   ((equal? (car w) #\a)
    (mooglp
     (weave-3 (cddr w))
     (weave-2 (cdr w))
     0.1))
   ((equal? (car w) #\b)
    (moogbp
     (weave-3 (cddr w))
     (weave-2 (cdr w))
     0.1))
   ((equal? (car w) #\c)
    (mooghp
     (weave-3 (cddr w))
     (weave-2 (cdr w))
     0.1))
   ((equal? (car w) #\d)
    (formant
     (weave-3 (cddr w))
     (weave-2 (cdr w))
     0.1))))






(card-loom-run loom
               '(("weave_forward" 4)
                 ("twist" 1 2 3 5)
                 ("weave_back" 4)
                 ("weave_forward" 4)
                 ("rotate_forward" 0 4 5)
                 ("weave_forward" 4)
                 ("weave_back" 4)

                 ))
