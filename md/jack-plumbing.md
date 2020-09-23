JACK-PLUMBING(1)
================

NAME
----
jack-plumbing - JACK Plumbing Daemon

SYNOPSIS
--------
jack-plumbing [options] [rule-files]

OPTIONS
-------
*-d*
:   Do not operate as a daemon.

*-o*
:   Do not load ordinary rule files.

*-q*
:   Quiet operation.

*-u*
:   Set the time, in micro-seconds, that must elapse after a
    connection notification to indicate the end of a notification set
    (default=30000, ie. 0.03 seconds).  This is an optimization switch.

DESCRIPTION
-----------
jack-plumbing maintains a set of port connection rules and manages
these as clients register ports with JACK.  Port names are implicitly
bounded regular expressions and support sub-expression patterns.

The rules are:

    (connect "SuperCollider:out_(.*)" "system:playback_\1")

This connect rule ensures that whenever scsynth(1) is running any
output ports it registers are connected to appropriate ALSA playback
ports.  The connect rule only makes a connection if none already
exist.

    (disconnect ".*" ".*")

This disconnect rule will delete every existing JACK connection.

<!--
    (also-connect "system:playback_1" "jack-udp-[0-9]*:in_1")

This also-connect rule ensures that when jack-udp(1) is running in
send mode all signals that are ordinarily sent to the local audio
interface will also be sent to the network destination.  The
also-connect aliasing applies to both the left and right hand side of
connect rules.

    (connect-exclusive "SuperCollider:out_(.*)" "ardour:sc3_in_\1/in 1")

This connect-exclusive rule ensures that if SuperCollider and an
ardour(1) session with appropriate tracks are both running that
SuperCollider gets connected to ardour and disconnected from
everything else.
-->

This type of connection daemon has the following advantages over both
environment variables and command line arguments:

* it does not require any client connection support or logic at all
* it provides a uniform user interface to all clients
* it has a somewhat declarative style

This type of daemon is very lightweight and non-intrusive: it requires
no arguments, it runs from an unprivileged thread, if the rule set is
empty it is a no-op, it does not write any files, it does not require
any modification to clients or to how clients are started.

When a set of port registrations is made it checks the modification
time of the rule set files, '/etc/jack-plumbing' and
'~/.jack-plumbing', and any files specified by the user, rereads the
rule set if it has been updated, then makes the JACK graph obey the
rules.

Any lines beginning with a semi-colon are ignored.

The rule set is sorted, disconnect rules are applied first, then
connect rules.

FILES
-----
* /etc/jack-plumbing
* ~/.jack-plumbing

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, July 2003

SEE ALSO
--------
jackd(1)
