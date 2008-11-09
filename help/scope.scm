(import (sosc))

(define fd
  (udp:open "127.0.0.1" 57140))

(define set-scope
  (lambda (cmd arg)
    (send fd (list cmd arg))))

(set-scope "/frames" 1024)
(set-scope "/delay" (/ 1000.0 24.0))

(set-scope "/mode" "signal")
(set-scope "/style" "fill")
(set-scope "/style" "line")
(set-scope "/style" "dot")

(set-scope "/mode" "embed")
(set-scope "/incr" 0.1)
(set-scope "/embed" 64)

(udp:close fd)
