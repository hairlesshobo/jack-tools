(import (sosc)
        (rsc3))

(define with-jackdl
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57190))
           (r (f fd)))
      (udp:close fd)
      r)))

(define g-load
  (lambda (i s)
    (message "/g_load" (list i s))))

(define c-set1
  (lambda (i n)
    (message "/c_set" (list i n))))

(define g-ctl
  (lambda (g i)
    (+ (* g 3) i)))

(define set-sin
  (lambda (g f a p)
    (with-jackdl
     (lambda (fd)
       (send fd (c-set1 (g-ctl g 0) f))
       (send fd (c-set1 (g-ctl g 1) a))
       (send fd (c-set1 (g-ctl g 2) p))))))

(with-jackdl
  (lambda (fd)
    (for-each
     (lambda (g)
       (send fd (g-load g "/home/rohan/sw/rju/help/sin.so")))
     (list 0 1 2))))

(set-sin (i-random 0 3)
         (random 220 880)
         (random 0.1 0.25)
         (random 0 1))

(with-jackdl
  (lambda (fd)
    (send fd quit)))
