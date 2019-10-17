import Control.Monad {- base -}

import Sound.OSC {- hosc -}

jack_osc_p :: Int
jack_osc_p = 57130

jack_osc_udp :: IO UDP
jack_osc_udp = openUDP "127.0.0.1" jack_osc_p

with_jack_osc :: Connection UDP t -> IO ()
with_jack_osc = withTransport_ jack_osc_udp

printer :: Connection UDP t
printer = do
  sendMessage (message "/receive" [int32 0x10])
  forever (do m <- recvMessage_err
              liftIO (print m))

main :: IO ()
main = with_jack_osc printer

{-

c <- withsc3 openUDP "127.0.0.1" p
sendPacket c (p_message "/recevive" [int32 0x01])

-}
