
(define with-jackdl
  (lambda (f)
    (let* ((fd (udp:open "127.0.0.1" 57190))
           (r (f fd)))
      (udp:close fd)
      r)))

(with-jackdl
  (lambda (fd)
    (send fd (c-set1 0 (random 220 880)))
    (send fd (c-set1 1 (random 0.1 0.5)))
    (send fd (c-set1 2 (random 0 1)))))

(with-jackdl
  (lambda (fd)
    (send fd quit)))
