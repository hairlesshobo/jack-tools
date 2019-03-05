import Data.Word {- base -}

import Sound.OSC {- hosc -}

import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

lxvst_default_port :: Int
lxvst_default_port = 57210

lxvst_default_udp :: IO UDP
lxvst_default_udp = openUDP "127.0.0.1" lxvst_default_port

with_lxvst :: Connection UDP a -> IO a
with_lxvst = withTransport lxvst_default_udp

lxvst_param :: Int -> Double -> Message
lxvst_param k n = message "/param" [int32 k,float n]

-- > to_lxvst (lxvst_midi [0xC0,19,0])
lxvst_midi :: [Word8] -> Message
lxvst_midi b = message "/midi" [Blob (blob_pack b)]

to_lxvst :: Message -> IO ()
to_lxvst = with_lxvst . sendMessage

-- > lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/ROM1A.syx"
-- > lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/vrc/VRC-101-A.syx"
lxvst_load_sysex :: FilePath -> IO ()
lxvst_load_sysex fn = do
  syx <- dx7_read_fmt9_sysex fn
  to_lxvst (lxvst_midi syx)
