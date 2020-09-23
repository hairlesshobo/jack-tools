JACK-SCOPE(1)
=============

NAME
----
jack-scope - JACK Oscilloscope

SYNOPSIS
--------
jack-scope [options]

OPTIONS
-------
*-b*
:   Set the frame size of the data displayed by the scope (default=512).

*-c*
:   Set colour mode grey|ega64 (default=grey)

*-d*
:   Set the delay interval in milli-seconds between scope updates (default=100).

*-e*
:   Set embedding delay in frames (default=6)

*-f*
:   Request that a still image file of each frame be stored to the indicated directory (default=nil)

*-g*
:   Set input gain (default=1)

*-h*
:   Set the scope height in pixels (default=512).

*-m*
:   Set the scope operating mode (default=signal).
    The operating modes are: signal, embed, hline.

*-n*
:   Set the number of channels (default=1).
    This sets the number of JACK input ports.
    Multiple channels are superimposed, each channel is drawn in a distinct colour.
    There is a compile time channel limit.

*-p*
:   A pattern describing the JACK ports to connect to, ie. `SuperCollider:out_%d` (default=nil).
    If this is not set consults the environment variable `JACK_SCOPE_CONNECT_TO`.

*-s*
:   Set the drawing style for signal mode (default=dot).

*-u*
:   Set the UDP port number to listen for OSC packets on (default=57140).

*-U*
:   Do not generate unique jack client name (ie. do not append PID)

*-w*
:   Set the scope width in pixels (default=512).

*-z*
:   Do not align to zero-crossing

DESCRIPTION
-----------
jack-scope is an oscilloscope for JACK under X11.  jack-scope draws
either a time domain signal trace or a self correlation trace.
Multiple input channels are superimposed, each channel is drawn in a
different colour.  jack-scope accepts OSC packets for interactive
control of drawing parameters.

The operating mode of jack-scope is set using *-m*.  In signal mode
jack-scope draws a time domain signal trace, in embed mode jack-scope
draws a self correlation trace, in hline mode an _optical sound_ picture
is made.

The size of the jack-scope window is set using *-h* and *-w*.  The
window is of fixed size and has centered gravity.  The time interval
that is displayed is determined by the frame size, set using *-b*.
The image refresh rate is determined by the delay interval, set using
*-d*.  Note that the interval is truncated to the nearest frame
boundary and that the time taken to compose the image and blit to the
screen is indeterminate.

The number of JACK input ports that jack-scope creates and monitors is
set using *-n*.  Multiple channels are drawn in superimposition, each
channel is drawn in a distinct colour.

In signal mode the trace is drawn in a style set using `/style`.  In
`dot` mode only the sample pixel in each column is drawn.  In `fill` mode
all pixels between the sample pixel and the zero pixel of each column
are drawn.  In `line` mode all pixels between the adjacent sample pixels
of each column are drawn.

In embed mode the trace is a self correlation signal with a sample
delay set using `/embed`.  The delayed sample is on the x-axis.  The
interpolation increment is set using `/incr`, increment values less
than one result in increasingly continuous trace paths.

In hline mode the data block is resampled to the window size and each
sample is drawn as an 8-bit greyscale horizontal line.  If the sample
rate is 48000, the block size is 2400, the window size is 800 and the
image delay is 50 then each frame is a consecutive drawing of 1/20 of
a second of the input signal.  A block size of 1920 and window size of
640 and delay of 40 gives consecutive drawings at 1/25 of a second.

    jack-scope -m hline -n 1 -b 2400 -h 800 -w 1280 -d 50 -p jack-play:out_%d
    jack-scope -m hline -n 2 -b 1920 -h 640 -w 640 -d 40 -p jack-play:out_%d

The hscan mode is closely related, the block is resampled (if
required) to the size of the window.

    jack-scope -g 2 -m hscan -w 200 -h 200 -d 100 -b 40000 -p jack-play:out_%d

jack-scope can store the animation as a sequence of uncompressed
ppm(5) image files.  To request this use the *-f* option with the
directory files should be written to as the argument.

The OSC messages understood by jack-scope are given in the table
below.  Each command requires one argument of the indicated type.  The
last column gives the option that sets the same parameter.

    Command      Description        Argument    Option
    -------      -----------        --------    ------
    /colour-mode colour mode        string      -c
    /delay       refresh-interval   float       -d
    /embed       embedding          int         -e
    /frames      frame-size         int         -b
    /incr        increment          float
    /input-gain  input gain         float       -g
    /mode        drawing-mode       string      -m
    /print       print state        int
    /style       style              string      -s

REFERENCES
----------
Monro, G. and Pressing, J.  "Sound Visualization Using Embedding: The
Art and Science of Auditory Autocorrelation" _CMJ_, 22/2, 1998.

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, January 2004

SEE ALSO
--------
jackd(1), X(7x), OSC(7) <http://opensoundcontrol.org/>
