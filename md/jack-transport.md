JACK-TRANSPORT(1)
=================
Rohan Drape <rd@slavepianos.org>


NAME
----
jack-transport - Text console jack transport interface

SYNOPSIS
--------
jack-transport

DESCRIPTION
-----------
jack-transport is a jack transport control interface using
ncurses.  It displays the transport state and current time, and
provides standard operating keys.

COMMANDS
--------
*s*
:   Start and stop transport.  Aliased to [space].

*l*
:   Locate  to  entered  time.   Starting to type a number will also
    enter locate mode.

*i*
:   Set forward & backward increment to entered interval (default = 5 seconds).

*z*
:   Locate to start (zero).

*r*
:   Erase and refresh screen.

*f*
:   Move forwards by increment.  Aliased to [>] and [right-arrow].

*b*
:   Move backwards by increment.  Aliased to [<] and [left-arrow].

*F*
:   Move forwards one minute.  Aliased to [.] and [up-arrow].

*B*
:   Skip backwards one minute.  Aliased to [,] and [down-arrow].

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>

SEE ALSO
--------
jackd(1), jack-play(1)
