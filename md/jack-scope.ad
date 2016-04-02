JACK-SCOPE(1)
=============
Rohan Drape <rd@slavepianos.org>


NAME
----
jack-scope - JACK Oscilloscope

SYNOPSIS
--------
jack-scope [options]

OPTIONS
-------
*-b*
:   Set the frame size of the data displayed by the scope
    (default=512).

*-d*
:   Set the delay interval in milli-seconds between scope updates
    (default=100.0).

*-f*
:   Request that a still image file of each frame be stored to the
    indicated directory.

*-m*
:   Set the scope operating mode (default=signal).  The operating
    modes are: signal and embed.

*-n*
:   Set the number of channels, and therefore the number of JACK input
    ports (default=1).  Multiple channels are superimposed, each
    channel is drawn in a distinct color.  There is a compile time
    channel limit.

*-p*
:   A pattern describing the JACK ports to connect to,
    ie. `SuperCollider:out_%d` (default=nil).  If this is not set
    consults the environment variable `JACK_SCOPE_CONNECT_TO`.

*-s*
:   Set the drawing style for signal mode (default=dot).

*-u*
:   Set the UDP port number to listen for OSC packets on (default=57140).

*-w*
:   Set the scope size in pixels (default=512).  The scope window is
    square.

DESCRIPTION
-----------
jack-scope is an oscilloscope for JACK under X11.  jack-scope draws
either a time domain signal trace or a self correlation trace.
Multiple input channels are superimposed, each channel is drawn in a
different color.  jack-scope accepts OSC packets for interactive
control of drawing parameters.

The operating mode of jack-scope is set using *-m*.  In signal mode
jack-scope draws a time domain signal trace, in embed mode jack-scope
draws a self correlation trace.

The size of the jack-scope window is set using *-w*, the scope window
is square.  The window is of fixed size and has centered gravity.  The
time interval that is displayed is determined by the frame size, set
using *-b*.  The image refresh rate is determined by the delay
interval, set using *-d*.  Note that the interval is truncated to the
nearest frame boundary and that the time taken to compose the image
and blit to the screen is indeterminate.

The number of JACK input ports that jack-scope creates and monitors is
set using *-n*.  Multiple channels are drawn in superimposition, each
channel is drawn in a distinct color.

In signal mode the trace is drawn in a style set using `/style`.  In
`dot` mode only the sample pixel in each column is drawn.  In `fill` mode
all pixels between the sample pixel and the zero pixel of each column
are drawn.  In `line` mode all pixels between the adjacent sample pixels
of each column are drawn.

In embed mode the trace is a self correlation signal with a sample
delay set using `/embed`.  The delayed sample is on the x-axis.  The
interpolation increment is set using `/incr`, increment values less
than one result in increasingly continuous trace paths.

jack-scope can store the animation as a sequence of uncompressed
ppm(5) image files.  To request this use the *-f* option with the
directory files should be written to as the argument.

The OSC messages understood by jack-scope are given in the table
below.  Each command requires one argument of the indicated type.  The
last column gives the option that sets the same parameter.

Command   Description        Argument    Option
-------   -----------        --------    ------
/mode     drawing-mode       string      -m
/style    style              string      N/A
/frames   frame-size         integer     -b
/delay    refresh-interval   float       -d
/embed    embedding          integer     N/A
/incr     increment          float       N/A

jack-scope implements no connection logic, use jack-plumbing(1)
instead.

REFERENCES
----------
Monro, G. and Pressing, J.  ‘‘Sound Visualization Using Embedding: The
Art and Science of Auditory Autocorrelation’’ CMJ, 22/2, 1998.

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>

SEE ALSO
--------
jackd(1), X(7x), OSC(7) <http://opensoundcontrol.org/>
