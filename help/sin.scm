(define with-jackdl
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57190))
           (r (f fd)))
      (udp:close fd)
      r)))

(define set-sin
  (lambda (f a p)
    (with-jackdl
     (lambda (fd)
       (send fd (c-set1 0 f))
       (send fd (c-set1 1 a))
       (send fd (c-set1 2 p))))))

(set-sin (random 220 880)
         (random 0.1 0.5)
         (random 0 1))

(with-jackdl
  (lambda (fd)
    (send fd quit)))
