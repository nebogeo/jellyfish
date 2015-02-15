;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; maths hacks, move to fluxa

(define (all-numbers? l)
  (cond
   ((null? l) #t)
   ((not (number? (car l))) #f)
   (else (all-numbers? (cdr l)))))

(define (+ . args)
  (if (all-numbers? args)
      (apply + args)
      (proc-list add args)))

(define (- . args)
  (if (all-numbers? args)
      (apply - args)
      (proc-list sub args)))

(define (/ . args)
  (if (all-numbers? args)
      (apply / args)
      (proc-list div args)))

(define (* . args)
  (if (all-numbers? args)
      (apply * args)
      (proc-list mul args)))

(define (proc-list p l)
  (cond
   ((eq? (length l) 1) (car l))
   ((eq? (length l) 2) (p (car l) (cadr l)))
   (else (p (car l) (proc-list p (cdr l))))))
