JACK-OSC(1)
===========

NAME
----
jack-osc - JACK Transport Publication Daemon

SYNOPSIS
--------
jack-osc [options]

OPTIONS
-------
*-c*
:   Set the drift correction interval in periods (default=64).

*-p*
:   Set the port number (default=57130).

DESCRIPTION
-----------
jack-osc publishes the transport state of the local JACK server as OSC
packets over a UDP connection.  jack-osc allows any OSC enabled
application to act as a JACK transport client, receiving sample
accurate pulse stream timing data, and monitoring and initiating
transport state change.

Clients request to receive timing and change notification packets by
sending a "request notification" packet, '/receive', to the jack-osc
server.  This packet has the form

    /receive category:i

where category is a bit mask that indicates what categories of
notification packets are requested.  The bit locations are:

    Category            Mask
    --------            ----
    REQUEST_TICK        0x0000001
    REQUEST_PULSE       0x0000002
    REQUEST_CORRECTION  0x0000004
    REQUEST_TRANSPORT   0x0000008
    REQUEST_TIME        0x0000016
    REQUEST_ALL         0xFFFFFFF

Clients request notification messages to be sent to an address that is
not that of the packet that requests the notification by sending a
"request notification at" packet, '/receive_at'.  This packet has the
form

    /receive_at category:i port-number:i host-name:s

where category is as for '/receive' and where port-number and
host-name give the address that notification should be sent to.

Once a client is registered subsequent '/receive' and '/receive_at'
messages edit the category value for that client.  To delete the
client from the register send a request with a category value of
negative one.

After requesting notification the client will receive all relevant
timing packets sent by the server.

The simplest time data is sent as:

/time loc:d
:   Transport time (in seconds).

All other jack-osc timing packets are
sent at the start of a JACK period as OSC message and have the same
shape:

    tag ntp:t utc:d frm:h arg...

where tag is the command name, and ntp, utc and frm are time stamps
that indicate the same time point, the start of the JACK period when
the packet was sent.  ntp is an unsigned 64bit integer NTP value.  utc
is a double precision real valued representation of the UTC time.  frm
is a signed 64bit integer frame counter the absolute value of which is
not defined but which increments synchronously with the ntp and utc
time stamps.  arg...  is the set of tag specific arguments.

The timing packets sent by jack-osc are:

/pulse ntp:t utc:d frm:h p-ntp:t p-utc:d p-frm:h pulse:i
:   Pulse Location.  This packet indicates that the nearest frame to
    the integer pulse pulse occurs at the time given by the time
    stamps p-ntp, p-utc and p-frm.  The pulse number is one based.
    This packet is sent at the start of the JACK period in which the
    integer pulse will occur.  This packet is not sent if the
    transport is stopped.  This packet is sent before the '/tick'
    packet for the same period.

/tick ntp:t utc:d frm:h frame:h pulse:d
:   Period Tick.  This packet is sent once per JACK period.  The
    integer value frame is the transport location in frames, the
    double precision real value pulse is the transport location in
    pulses.  The pulse value is read from an accumulator and is
    approximate only, the accumulator is corrected at each integer
    pulse location.

/drift ntp:t utc:d frm:h ntp-dif:h utc-dif:d
:   Drift Correction.  This packet is sent whenever the clock drift
    correction is run.  The frequency of this is set by the *-c*
    option to the jack-osc server.  The integer value ntp-dif is the
    NTP form of the corrected drift value and utc-dif the UTC form.
    Since JACK is a sample clock there is no frame drift value.

The state change packets sent by jack-osc are:

/transport ntp utc frm fps ppm ppc pt state
:   Transport state change.  This packet is sent whenever the JACK
    transport changes.  The double precision real value fps is the
    sample rate in frames per second.  The double precision real value
    ppm is the tempo in pulses per minute.  The double precision real
    value ppc is the measure length in pulses per cycle.  The double
    precision real value pt is the pulse type.  The integer value
    state is zero if the transport has stopped and one if it has
    started.  See also the '/status' message described below.

Clients can request the current frame and pulse values by sending a
'/current' packet, which requires no argument.  The replies with a
'/current.reply' packet, which has the same form as a '/tick' packet.
The precise interpretation of packets acquired in this manner is
problematic.

Clients request a status packet by sending a "request status" packet,
'/status', which requires no argument.  The server replies immediately
with a status reply packet, '/status.reply'.  The status packet is an
OSC message and is not timestamped.  It has the shape:

    /status.reply fps ppm ppc pt state

where the argument values are as described for the '/transport'
message.  It is intended that a client will request a single status
packet before requesting notification for all subsequent state
changes.

Clients initiate a change in transport roll state by sending a
"request transport operation" packet, '/start' or '/stop' to the
jack-osc server.  Neither requires an argument.

Clients initiate a change in transport location by sending a "request
locate operation" packet, '/locate', to the jack-osc server.  It has
the shape:

    /locate location

where the single precision real value location is the requested
transport location in seconds.

Clients can connect and disconnect ports by sending '/connect' and
'/disconnect' messages to the jack-osc server.  Both have the shape:

   /[dis]connect left right

jack-osc implements only a subset of the OSC protocol.  In particular
it does not implement the patten matching rules and does not implement
a scheduler for incoming messages.

jack-osc drops all unrecognized incoming packets.

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, January 2004

SEE ALSO
--------
jackd(1), OSC(7) <http://opensoundcontrol.org/>
