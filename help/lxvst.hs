import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

import Sound.RJU.LXVST {- rju -}

{-

dir = ""/home/rohan/sw/hsc3-data/data/yamaha/"
ld fn = lxvst_load_sysex (dir ++ fn)

ld "dx7/rom/DX7-ROM1A.syx"
ld "dx7/vrc/VRC-102-B.syx"
ld "dx1/DX1-B2.syx"
ld "tx816/TX816-TFR2.syx"
ld "tx7/TX7-B.syx"
ld "dx7s/DX7S-INTB.syx"
ld "dx7ii/rom/DX7II-64B.syx"
ld "dx7ii/vrc/VRC-1002-A-1.syx"
ld "dx7/ext/avic/KV06A.syx"
ld "dx7/ext/musicdata/ciani.syx"
ld "dx7/ext/rittor/fukuda.syx"

-}
lxvst_load_sysex :: FilePath -> IO ()
lxvst_load_sysex fn = do
  Just syx <- dx7_read_fmt9_sysex fn
  to_lxvst [lxvst_midi syx]
