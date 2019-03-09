module Sound.RJU.LXVST where

import Data.Word {- base -}

import Sound.OSC {- hosc -}

lxvst_default_port :: Int
lxvst_default_port = 57210

lxvst_default_udp :: IO UDP
lxvst_default_udp = openUDP "127.0.0.1" lxvst_default_port

with_lxvst :: Connection UDP a -> IO a
with_lxvst = withTransport lxvst_default_udp

lxvst_param :: Int -> Double -> Message
lxvst_param k n = message "/param" [int32 k,float n]

lxvst_midi :: [Word8] -> Message
lxvst_midi b = message "/midi" [Blob (blob_pack b)]

to_lxvst :: Message -> IO ()
to_lxvst = with_lxvst . sendMessage
