import Control.Monad {- base -}

import Sound.OSC {- hosc -}

rju_osc_p :: Int
rju_osc_p = 57130

rju_osc_udp :: IO UDP
rju_osc_udp = openUDP "127.0.0.1" rju_osc_p

with_rju_osc :: Connection UDP t -> IO ()
with_rju_osc = withTransport_ rju_osc_udp

printer :: Connection UDP t
printer = do
  sendMessage (message "/receive" [int32 0x10])
  forever (do m <- recvMessage_err
              liftIO (print m))

main :: IO ()
main = with_rju_osc printer

{-

c <- withsc3 openUDP "127.0.0.1" p
sendPacket c (p_message "/recevive" [int32 0x01])

-}
