RJU-LEVEL(1)
============

NAME
----
rju-level - JACK CLI level meter

SYNOPSIS
--------
rju-level [options]

OPTIONS
-------
*-k*
:   port number offset

*-n*
:   number of channels

*-p*
:   UDP port number

DESCRIPTION
-----------
rju-level is a JACK CLI level meter.

It writes peak and average levels as text fields using curses.

While running type 'z' to zero peak levels and 'q' to quit.

EXAMPLE
-------

    rju-level -n 2 -p SuperCollider:out_%d

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, April 2019

SEE ALSO
--------
jackd(1)
