-- | jack-lxvst
module Sound.RJU.LXVST where

import Data.Word {- base -}

import Sound.OSC {- hosc -}

-- | Default jack-lxvst UDP port number.
lxvst_default_port :: Int
lxvst_default_port = 57210

-- | UDP at local machine and default port.
lxvst_default_udp :: IO UDP
lxvst_default_udp = openUDP "127.0.0.1" lxvst_default_port

-- | 'withTransport' of 'lxvst_default_udp'
with_lxvst :: Connection UDP a -> IO a
with_lxvst = withTransport lxvst_default_udp

-- | User exit.
lxvst_exit :: Message
lxvst_exit = message "/exit" []

-- | Set VST parameter /k/ to value /n/.
lxvst_param :: Int -> Double -> Message
lxvst_param k n = message "/param" [int32 k,float n]

-- | Set VST program to /k/.
lxvst_program :: Int -> Message
lxvst_program k = message "/program" [int32 k]

-- | Sent VST midi message /b/.
lxvst_midi :: [Word8] -> Message
lxvst_midi b = message "/midi" [Blob (blob_pack b)]

-- | 'with_lxvst' of 'sendMessage'.
--
-- > to_lxvst lxvst_exit
to_lxvst :: Message -> IO ()
to_lxvst = with_lxvst . sendMessage
