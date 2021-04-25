RJU-DL(1)
=========

NAME
----
rju-dl - JACK shared library dsp loader

SYNOPSIS
--------
rju-dl [options]

OPTIONS
-------
*-b*
:   Set the number of buffers (default=8).

*-c*
:   Set the number of input and output channels (default=8).

*-k*
:   Set the number of control buses (default=64).

*-u*
:   Set the udp port number (default=57190).

DESCRIPTION
-----------
rju-dl loads dsp algorithms from shared libraries and allows user
interaction with the executing graph.  Commands are sent as OSC
packets over a UDP connection.

The dsp graph code must provide three functions:

    size_t (*dsp_memreq)();
    void (*dsp_init)(void *);
    void (*dsp_step)(void *,int);

rju-dl accepts the OSC commands:

    Command   Arguments                             Description
    -------   ---------                             -----------
    /b_alloc  id::int frames::int channels::int     buffer allocate
    /c_set    index::int value::float               control set
    /c_setn   index::int count:int value::float     control sequence set
    /g_load   object-file::file-path                graph load
    /g_unload                                       graph unload
    /quit                                           quit

rju-dl consults the `RJU_DL_CONNECT_TO` and `RJU_DL_CONNECT_FROM`
environment variables.

rju-dl implements only a subset of the OSC protocol.  In particular
it does not implement the patten matching rules and does not implement
a scheduler for incoming messages.

rju-dl drops all unrecognized incoming packets.

AUTHOR
------

Rohan Drape <rd@rohandrape.net>, October 2008-2021

SEE ALSO
--------
jackd(1), OSC(7) <http://opensoundcontrol.org/>
