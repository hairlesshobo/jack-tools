(import (sosc))

(define fd
  (udp:open "127.0.0.1" 57140))

(define set-scope
  (lambda (cmd arg)
    (send fd (list cmd arg))))

(set-scope "/frames" (/ 512 2))
(set-scope "/frames" (* 512 2))
(set-scope "/frames" (* 512 4))
(set-scope "/frames" (* 44100 1))
(set-scope "/delay" (/ 1000 25))
(set-scope "/delay" (/ 1000 10))

(set-scope "/mode" "signal")
(set-scope "/style" "fill")
(set-scope "/style" "line")
(set-scope "/style" "dot")

(set-scope "/mode" "embed")
(set-scope "/incr" 0.1)
(set-scope "/embed" 12)
(set-scope "/embed" 64)

(set-scope "/mode" "hline")
(set-scope "/colour-mode" "grey")
(set-scope "/colour-mode" "ega64")
(set-scope "/frames" 480)
(set-scope "/frames" (* 480 4))

(set-scope "/mode" "hscan")

(set-scope "/input-gain" 0.5)
(set-scope "/input-gain" 1.0)
(set-scope "/input-gain" 2.0)
(set-scope "/input-gain" 4.0)
(set-scope "/input-gain" 8.0)

(set-scope "/print" 1)

(udp:close fd)
