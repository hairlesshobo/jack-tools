import Sound.SC3.Data.Yamaha.DX7 {- hsc3-data -}

import Sound.RJU.LXVST {- rju -}

{-


to_lxvst (lxvst_program (10 - 1))

lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/vrc/VRC-102-B.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx1/DX1-B2.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/tx816/TX816-TFR2.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/tx7/TX7-B.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7s/DX7S-INTB.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/rom/DX7II-64B.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7ii/vrc/VRC-1002-A-1.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ext/avic/KV06A.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ext/musicdata/ciani.syx"
lxvst_load_sysex "/home/rohan/sw/hsc3-data/data/yamaha/dx7/ext/rittor/fukuda.syx"

-}
lxvst_load_sysex :: FilePath -> IO ()
lxvst_load_sysex fn = do
  syx <- dx7_read_fmt9_sysex fn
  to_lxvst (lxvst_midi syx)
