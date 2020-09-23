JACK-UDP(1)
===========

NAME
----
jack-udp - JACK UDP Transport Client

SYNOPSIS
--------
jack-udp [options] send|recv

OPTIONS
-------
*-b*
:   Set the ring buffer size in frames (default=4096).

*-c*
:   Set the client name (default=jack-udp-PID).

*-n*
:   Set the number of channels, and therefore the number of JACK ports
    (default=2).

*-p*
:   Set the port number (default=57160).

*-r*
:   The remote host name, for use in send mode (default="127.0.0.1").

DESCRIPTION
-----------
jack-udp is a UDP audio transport mechansim for JACK.  The send mode
reads signals from a set of JACK input ports and sends UDP packets to
the indicated port at the indicated host at a rate determined by the
local JACK daemon.  The recv mode reads incoming packets at the indi-
cated port and writes the incoming data to a set of JACK output ports
at a rate that is determined by the local JACK daemon.

This transport mechanism is unreliable.  Both send and recv clients
will report buffer overflow and underflow occurences, and recv clients
will report dropped and out-of-order packets, and shutdown on channel
mismatch packets.  In practice this mechanism can be made highly reli-
able over local networks.

jack-udp implements no connection logic, use jack-plumbing(1) instead.

EXAMPLE
-------

    192.0.0.1:~$ jack-udp -r 192.0.0.2 send
    192.0.0.2:~$ jack-udp recv

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>

SEE ALSO
--------
jackd(1)
