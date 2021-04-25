(import (sosc)
        (rsc3))

(define with-rju-dl
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57190))
           (r (f fd)))
      (udp:close fd)
      r)))

(define g-load
  (lambda (s)
    (message "/g_load" (list s))))

(define c-set1
  (lambda (i n)
    (message "/c_set" (list i n))))

(define set-sin
  (lambda (f a p)
    (with-rju-dl
     (lambda (fd)
       (send fd (c-set1 0 f))
       (send fd (c-set1 1 a))
       (send fd (c-set1 2 p))))))

(with-rju-dl
  (lambda (fd)
    (send fd (g-load "/home/rohan/sw/rju/help/sin.so"))))

(set-sin (random 220 880)
         (random 0.1 0.25)
         (random 0 1))

(with-rju-dl
  (lambda (fd)
    (send fd quit)))
