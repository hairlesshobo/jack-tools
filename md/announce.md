[narkive](https://narkive.com/DbRqr9mN.1)

~~~~
Subject: JACK Plumbing Daemon
From: Rohan Drape
Date: 2003-07-26 00:59:30 UTC

I have written a JACK plumbing daemon. It maintains a set of port
connection rules and manages these as clients register ports with
JACK. Port names are implicitly bounded regular expressions so that
clients that include a PID in their name can be managed. The rules:

(connect "SuperCollider:out_1" "alsa_pcm:playback_1")
(connect "SuperCollider:out_2" "alsa_pcm:playback_2")

ensure that whenever SC is running it is hooked up to the output
ports. The connect rule only makes a connection if none already
exist.

The rules:

(connect "SuperCollider:out_1" "bridge-[0-9]*:meter_1")
(connect "SuperCollider:out_2" "bridge-[0-9]*:meter_2")

provide a general solution to the meterbridge problem. The MB problem
is this: if SC is up and MB is started things work OK, however if SC is
stopped, MB keeps running, and then SC restarts, MB does not pick up
the new connection. The above rules ensure that SC is always hooked
up to MB whenever they are both up. If meterbridge makes only
ordinary input ports and implements no connection logic, as my patched
meterbridge in fact does, these rules still work.

The rules:

(connect-inputs-of "alsa_pcm:playback_1" "bridge-[0-9]*:meter_1")
(connect-inputs-of "alsa_pcm:playback_2" "bridge-[0-9]*:meter_2")

are hypothetical but would ensure that anything that connects to the
ALSA playback ports also hooks up to MB.

The rules:

(connect-exclusive "SuperCollider:out_1" "ardour:SC_1/in 1")
(connect-exclusive "SuperCollider:out_2" "ardour:SC_2/in 2")

do ensure that if SC and an ardour session with appropriate tracks
(SC_1 and SC_2) are both up that SC gets connected to ardour and
disconnected from everything else.

This type of daemon has the following advantages over both environment
variables and command line arguments: 1. it does not require any
client connection support or logic at all, 2. it provides a uniform
user interface to all clients, and 3. it has a somewhat declarative
style.

This type of daemon is very lightweight and non-intrusive: it requires
no arguments, it runs from an unprivileged thread, if the rule set is
empty it is a no-op, it does not write any files, it does not require
any modification to clients or to how clients are started.

When a set of port registrations is made it checks the modification
time of the rule set file, ~/.jackd.plumbing, re-reads the rule set if
it has been updated, then makes the JACK graph obey the rules.

The current implementation is simple minded, but it works well enough
to manage my environment. It relieve various minor JACK irritations.
The most serious problem with this implementation is that rules cannot
actually be written as S-Expressions, with strings quoted in the
normal manner, they are written as comma separated, very fragile,
unquoted strings (ie. 'connect,SC:out,MB:in'). The current parser is
a toy only. The second most serious problem is that rule sets are not
sorted, they must be written in sequence, that is connect-exclusive
rules must come after connect rules. The source is available at:

<http://www.alphalink.com.au/~rd/c/jackd.plumbing.c>

A modified jackctl script that also manages the plumbing daemon, they
go up and come down together, is available at:

<http://www.alphalink.com.au/~rd/c/jackctl>

(Note: I think this is modified in other ways, it reads all variables
in the form they are written as long options to jackd. It might not
work with your .jackrc file. BTW, I think some form of this script
should be included in JACK before 1.0.)

I am announcing this here because I think that 1. *anything* that
encourages JACK clients not to implement any connection logic at all
by default is a Good Thing (TM), 2. *anything* that provides a single
location where all connection logic is written to is also a Good
Thing, and 3. I have not seen a JACK connection manager of this sort.

Regards,
Rohan

NOTE ASIDE: The plumbing name is a reference to Rob Pikes 'plumbd'
system from Plan 9, which I was reminded of when writing this, though
the connection is oblique, see:

Rob Pike, "Plumbing and Other Utilities", Proceedings of 2000 USENIX
Annual Technical Conference, San Diego, California
~~~~

[narkive](https://jack-devel.jackaudio.narkive.com/Mv7EO0Tf/announce-jack-scope>)

~~~~
Date: 2004-01-20 00:26:01 UTC
Subject: Announce - jack.scope
From: Rohan Drape
To: jack-devel

jack.scope is an oscilloscope for JACK under X11. jack.scope draws
either a time domain signal trace or a self correlation trace.
Multiple input channels are superimposed, each channel is drawn in a
different color. jack.scope accepts OSC packets for interactive
control of drawing parameters.

For details of self correlation traces see: Monro, G. and Pressing, J.
"Sound Visualization Using Embedding: The Art and Science of Auditory
Autocorrelation" CMJ, 22/2, 1998.

jack.scope is available from <http://www.alphalink.com.au/~rd>.
~~~~

[linuxaudio](https://lists.linuxaudio.org/archives/linux-audio-announce/2004-March/000340.html)

~~~~
Subject: jack.plumbing - Announce
Date: Sat Mar 13 20:27:17 EST 2004
From: Rohan Drape <rd@alphalink.com.au>
To: linux-audio-announce

The JACK plumbing daemon has a new rule to dramatically reduce
ordinary rule set sizes, a new system wide configuration file, and a
new version number to indicate progress.

The rule 'also-connect' makes managing machines that connect to a
studio using an ethernet cable simpler.  Adding the entry:

(also-connect "alsa_pcm:playback_1"  "jack.udp-[0-9]*:in_1")

to ~/.jack.plumbing ensures that when jack.udp(1) is running in send
mode all signals that are ordinarily sent to the local audio interface
will also be sent to the network destination.  The also-connect
aliasing applies to both the left and right hand side of connect
rules, so that:

(also-connect "jack.udp-[0-9]*:out_\1" "alsa_pcm:capture_\1")

works as expected.

The changes required to support this rule exacerbate the loss of
efficiency that began with the introduction of sub-expression rules.
Again the reduction in rule set sizes is the compensation, however
JACK sessions with ~= 100 ports provide serious exercise for the
plumber.

The file /etc/jack.plumbing is now consulted if it exists.

A source archive and a copy of the manual page are available at:
<http://www.alphalink.com.au/~rd>

Regards,
Rohan
~~~~

[narkive](https://jack-devel.jackaudio.narkive.com/IXHwtUR3/ann-announce-jack-record)

~~~~
Subject: Announce jack.record
Date: 2004-04-04 03:26:59 UTC
From: Rohan Drape
To: jack-devel

jack.record is a lightweight JACK capture client to write an arbitrary
number of channels to disk.

Minor novelties in relation to the example capture client are: writing
either a single multiple channel file or multiple single channel
files, writing files in any libsndfile supported format, and an
encouraging absence of connection logic.

The jack.plumbing rule:

(also-connect "alsa_pcm:playback_\1" "jack.record-[0-9]*:in_\1")

will ensure that jack.record captures whatever you are hearing.

A source archive is available at <http://www.alphalink.com.au/~rd>.

Regards,
Rohan

--

rd
<http://www.alphalink.com.au/~rd>
~~~~

[linuxaudio](https://lists.linuxaudio.org/archives/linux-audio-announce/2004-December/000502.html)

~~~~
Subject: jack.osc
Date: Tue Dec 14 09:23:55 EST 2004
From: Rohan Drape <rd@alphalink.com.au>
To: linux-audio-announce

jack.osc is a JACK transport publication daemon

Changes ::

- name of project is changed from jack.clock.
- names of OSC packets are changed to be more human readable.
- all state notification is packed into a single OSC packet.

jack.osc publishes the transport state of the local JACK server as OSC
packets over a UDP connection.  jack.osc allows any OSC enabled
application to act as a JACK transport client, receiving sample
accurate pulse stream timing data, and monitoring and initiating
transport state change.

A source archive and documentation are available at:
<http://www.alphalink.com.au/~rd/>

Regards,
Rohan
~~~~

[linuxaudio](https://lists.linuxaudio.org/archives/linux-audio-announce/2004-December/000501.html)

~~~~
Subject: midi.osc - MIDI Packet Publication Daemon
Date: Tue Dec 14 09:23:17 EST 2004
From: Rohan Drape
To: linux-audio-announce

midi.osc publishes MIDI packets on the local host MIDI system as OSC
packets over a UDP connection.  midi.osc allows any environment that
supports OSC to act as a MIDI client, sending and receiving MIDI data,
and monitoring and initiating changes to the host MIDI system.

There is support for ALSA Sequencer (Linux) and CoreMidi (OSX) hosts.
There are some issues with the ALSA send support, any help on this
would be appreciated.

A source archive and documentation are available at:
<http://www.alphalink.com.au/~rd/>

Regards,
Rohan
~~~~

