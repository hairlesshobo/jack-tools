(define with-jackdl
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57190))
           (r (f fd)))
      (udp:close fd)
      r)))

(define g-load
  (lambda (i s)
    (message "/g_load" (list i s))))

(define p-set1
  (lambda (g i n)
    (message "/p_set1" (list g i n))))

(define set-sin
  (lambda (g f a p)
    (with-jackdl
     (lambda (fd)
       (send fd (p-set1 g 0 f))
       (send fd (p-set1 g 1 a))
       (send fd (p-set1 g 2 p))))))

(with-jackdl
  (lambda (fd)
    (send fd (g-load 0 "/home/rohan/sw/jack.*/help/sin.so"))
    (send fd (g-load 1 "/home/rohan/sw/jack.*/help/sin.so"))))

(set-sin (i-random 0 2)
         (random 220 880)
         (random 0.1 0.25)
         (random 0 1))

(with-jackdl
  (lambda (fd)
    (send fd quit)))
