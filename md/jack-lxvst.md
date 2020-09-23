JACK-LXVST(1)
=============

NAME
----
jack-lxvst - JACK CLI Linux/VST Host

SYNOPSIS
--------
jack-lxvst vst-file

OPTIONS
-------
*-h*
:   Help?

DESCRIPTION
-----------
jack-lxvst is a JACK CLI host for Linux/VST plugins.

It consults the environment variables JACK_LXVST_MIDI_CONNECT_FROM and JACK_LXVST_CONNECT_TO.

It listens for OSC messages (/midi and /param) at port 57210 (by default).

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, April 2016

SEE ALSO
--------
jackd(1)
