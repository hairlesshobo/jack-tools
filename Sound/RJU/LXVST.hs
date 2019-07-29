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
lxvst_set_param :: Int -> Double -> Message
lxvst_set_param k v = message "/set_param" [int32 k,float v]

-- | Set VST parameters from /k/ to values at /v/.
lxvst_set_param_seq :: Int -> [Double] -> Message
lxvst_set_param_seq k v = message "/set_param_seq" (int32 k : int32 (length v) : map float v)

-- | Print all VST parameters
lxvst_print_param :: Message
lxvst_print_param = message "/print_param" []

-- | Set VST program to /k/.
lxvst_set_program :: Int -> Message
lxvst_set_program k = message "/set_program" [int32 k]

-- | Sent VST midi message /b/.
lxvst_midi :: [Word8] -> Message
lxvst_midi b = message "/midi" [Blob (blob_pack b)]

-- * HL

lxvst_send_set_param :: Int -> Double -> IO ()
lxvst_send_set_param k = to_lxvst . return . lxvst_set_param k

lxvst_send_set_param_seq :: Int -> [Double] -> IO ()
lxvst_send_set_param_seq k = to_lxvst . return . lxvst_set_param_seq k

lxvst_send_print_param :: IO ()
lxvst_send_print_param = to_lxvst [lxvst_print_param]

-- > lxvst_send_midi [0x90,60,60]
-- > lxvst_send_midi [0x80,60,60]
lxvst_send_midi :: [Word8] -> IO ()
lxvst_send_midi = to_lxvst . return . lxvst_midi
