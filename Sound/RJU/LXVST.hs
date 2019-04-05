-- | jack-lxvst
module Sound.RJU.LXVST where

import Data.Word {- base -}

import Sound.OSC {- hosc -}

-- * IO

-- | Default jack-lxvst UDP port number.
lxvst_default_port :: Int
lxvst_default_port = 57210

-- | UDP at local machine and default port.
lxvst_default_udp :: IO UDP
lxvst_default_udp = openUDP "127.0.0.1" lxvst_default_port

-- | 'withTransport' of 'lxvst_default_udp'
with_lxvst :: Connection UDP a -> IO a
with_lxvst = withTransport lxvst_default_udp

-- | 'with_lxvst' of 'sendMessage'.
--
-- > to_lxvst lxvst_exit
to_lxvst :: [Message] -> IO ()
to_lxvst = with_lxvst . mapM_ sendMessage

-- * MSG

-- | User exit.
lxvst_exit :: Message
lxvst_exit = message "/exit" []

-- | Set VST parameter /k/ to value /v/.
lxvst_param :: Int -> Double -> Message
lxvst_param k v = message "/param" [int32 k,float v]

-- | Set VST parameters from /k/ to values at /v/.
lxvst_param_n :: Int -> [Double] -> Message
lxvst_param_n k v = message "/param_n" (int32 k : int32 (length v) : map float v)

-- | Set VST program to /k/.
lxvst_program :: Int -> Message
lxvst_program k = message "/program" [int32 k]

-- | Sent VST midi message /b/.
lxvst_midi :: [Word8] -> Message
lxvst_midi b = message "/midi" [Blob (blob_pack b)]

-- * HL

-- | 'to_lxvst' of 'lxvst_param'.
lxvst_set_param :: Int -> Double -> IO ()
lxvst_set_param k = to_lxvst . return . lxvst_param k

-- | 'to_lxvst' of 'lxvst_param_n'.
lxvst_set_param_n :: Int -> [Double] -> IO ()
lxvst_set_param_n k = to_lxvst . return . lxvst_param_n k

-- | 'to_lxvst' of 'lxvst_midi'.
lxvst_send_midi :: [Word8] -> IO ()
lxvst_send_midi = to_lxvst . return . lxvst_midi
