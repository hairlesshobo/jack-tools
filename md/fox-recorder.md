fox-recorder(1)
=============

Name
----
fox-recorder - Multitrack JACK audio recorder

SYNOPSIS
--------
rju-record [options] output_pattern

OPTIONS
-------
*-b*
:   Set the disk buffer size in frames (default=4096).  This value
    must be a power of two.  If the JACK period size at any time
    exceeds this value the recorder will halt.

*-f*
:   Set the file format (default=0x10006).  The number is a bitwise-or
    of two values, the first describes the file type, the second
    the data type.  The default value of 0x10000 | 0x00006 describes a
    thirty-two bit floating point WAV file.  0x2 is 16-bit and 0x3 24-bit.

*-m*
:   Set the minimal disk transfer size in frames (default=32).  This
    is an optimization switch.

*-n*
:   Set the number of input channels to create (default=2).

*-o*
:   Integer offset for port connection pattern (default=0).

*-p*
:   A pattern describing the JACK ports to connect to,
    ie. SuperCollider:out_%d (default=nil).

*-s*
:   Capture to a set of single channel sound files.  The sound file
    name must be a valid template.

*-t*
:   Set a timer to stop the recording after the specified number of
    seconds (default=-1).

DESCRIPTION
-----------
rju-record is a light-weight JACK capture client to write an
arbitrary number of channels to disk.

If the default mode a single multiple channel sound file is written.
If the *-s* option is given a set of single channel sound files are
written.  In this case the sound file name must be a valid template,
the substring %d is replaced with the channel number counting from
zero.

rju-record will write files in any format supported by libsndfile.
The table below shows the most common file format masks.  For other
values see the file 'sndfile.h'.

    Format             Code      Description
    ------             ----      -----------
    SF_FORMAT_WAV      0x10000   Microsoft WAV format
    SF_FORMAT_AIFF     0x20000   Apple/SGI AIFF format
    SF_FORMAT_PCM_16   0x00002   Signed 16 bit data
    SF_FORMAT_PCM_24   0x00003   Signed 24 bit data
    SF_FORMAT_PCM_32   0x00004   Signed 32 bit data
    SF_FORMAT_FLOAT    0x00006   32 bit float data

AUTHOR
------
Rohan Drape <http://rd.slavepianos.org/>, April 2004

SEE ALSO
--------
jackd(1), libsndfile(3) <http://mega-nerd.com/libsndfile/>
