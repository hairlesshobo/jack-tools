import Sound.OSC {- hosc -}

lxvst_default_udp :: IO UDP
lxvst_default_udp = openUDP "127.0.0.1" 57210

with_lxvst :: Connection UDP a -> IO a
with_lxvst = withTransport lxvst_default_udp

-- | Zero-indexed.
lxvst_pgm_set :: Int -> Message
lxvst_pgm_set k = message "/pgm_set" [int32 k]

lxvst_p_set :: Int -> Double -> Message
lxvst_p_set k n = message "/p_set" [int32 k,float n]

{-
with_lxvst (sendMessage (lxvst_pgm_set 11))
-}
