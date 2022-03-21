rju
---

[jackd](http://jackaudio.org/) utilities

- [rju-data][-data]: jack audio data onto osc (May 2016)
- [rju-dl][-dl]: load dsp algorithms from shared libraries (October 2008)
- [rju-level][-level]: jack cli level meter (April 2019)
- [rju-lxvst][-lxvst]: jack cli host for linux vst instruments (April 2016)
- [rju-osc][-osc]: jack <-> [open sound control](http://opensoundcontrol.org/) daemon (January 2004)
- [rju-play][-play]: [resampling](http://mega-nerd.com/libsamplerate/) [soundfile](http://mega-nerd.com/libsndfile/) playback (November 2003)
- [rju-plumbing][-plumbing]: [plumbing](https://9fans.github.io/plan9port/man/man7/plumb.html) daemon (July 2003)
- [rju-record][-record]: soundfile recording (April 2004)
- [rju-scope][-scope]: plain [X](http://x.org) oscilloscope (January 2004)
- [rju-transport][-transport]: minimalist [ncurses](http://gnu.org/software/ncurses/) jack transport (November 2006)
- [rju-udp][-udp]: jack over udp client (November 2003) [c.f. [netjack](http://netjack.sf.net/)]

<!-- [plumb]: http://plan9.bell-labs.com/sys/doc/plumb.html -->

[-data]: https://rohandrape.net/?t=rju&e=md/rju-data.md
[-dl]: https://rohandrape.net/?t=rju&e=md/rju-dl.md
[-level]: https://rohandrape.net/?t=rju&e=md/rju-level.md
[-lxvst]: https://rohandrape.net/?t=rju&e=md/rju-lxvst.md
[-osc]: https://rohandrape.net/?t=rju&e=md/rju-osc.md
[-play]: https://rohandrape.net/?t=rju&e=md/rju-play.md
[-plumbing]: https://rohandrape.net/?t=rju&e=md/rju-plumbing.md
[-record]: https://rohandrape.net/?t=rju&e=md/rju-record.md
[-scope]: https://rohandrape.net/?t=rju&e=md/rju-scope.md
[-transport]: https://rohandrape.net/?t=rju&e=md/rju-transport.md
[-udp]: https://rohandrape.net/?t=rju&e=md/rju-udp.md

To build type:

> make prefix=~/opt all install

<!--

Documentation is in [asciidoc](http://methods.co.nz/asciidoc/) format, which is more or less
interoperable with [markdown](http://daringfireball.net/projects/markdown/) and here has an `.md` suffix. To build type:

> cd md ; for i in *.md ; do asciidoc $i; done
-->

tested-with:
[gcc](http://gcc.gnu.org/)-10.2.1
[clang](https://clang.llvm.org/)-11.0.1

debian: liblo-dev libncurses-dev (c.f. r-common/README)

These were all initially named with a `jack-` prefix, and were renamed April 2021.

© [rohan drape](http://rohandrape.net/),
  2003-2022,
  [gpl](http://gnu.org/copyleft/).
  with contributions by:

- Nikita Zlobin, <!-- cook60020tmp@mail.ru --> 2016-05-14
- Yoran Heling, <!-- projects@yorhel.nl --> 2010-10-30
- Jeremy Hall, <!-- jhall@maoz.com --> 2010-09-07
- Tristan Matthews, <!-- le.businessman@gmail.com --> 2009-10-10
- Ignacio Sánchez, <!-- ignacio.sanchez@vocali.net --> 2009-02-20
- Karsten Gebbert, <!-- k.gebbert@gmail.com --> 2008-06-27

see the [history](https://rohandrape.net/?t=rju&q=history) for details

initial [announce](https://rohandrape.net/?t=rju&e=md/announce.md)ments
