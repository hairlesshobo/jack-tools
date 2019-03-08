JACK-DATA(1)
============
<rd@slavepianos.org>

NAME
----
jack-data - JACK audio data onto OSC

SYNOPSIS
--------
jack-data nc read write type udp

OPTIONS
-------
*-h*
:   help?

DESCRIPTION
-----------

`nc` is the number of input channels, `read` is the input block size
in frames, `write` is the output block size (also in frames), `type`
is output data type (either `u8` or `f32`), `udp` is the port number
to send OSC packets to.  The packets are sent as `/data` messages
with the first four arguments (ie. nc, read, write and type) followed
by the resampled (if required) interleaved audio data.


    $ jack-data 1 512 32 u8 57190 &
    $ hosc-print json -p 57190
    ["/data",1,512,32,"u8",{"blob":[0,3,7,9,10,7,1,6,14,16,12,3, [...]
    ^C
    $


SEE ALSO
--------
jackd(1), OSC(7) <http://opensoundcontrol.org/>
