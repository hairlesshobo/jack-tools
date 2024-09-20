# FoxRecorder

## About

FoxRecorder is a simple CLI utility for recording multitrack audio straight to disk as either WAV or BWF format files. It requires that the [JACK audio server](http://jackaudio.org/) be installed and running when executed.

## Purpose

I frequently run live production and need a simple way to record multi track audio direct out of a digital sound board. Now, many people have this ability directly in the board to write to a SD card - but I do not. At the time of this writing, I am primarily mixing on a number of X32 boards that do not have the X-LIVE card installed, and I am not about to take one with me in my gig bag and install it in every board I use, so I opted to (sorta) create this - more on this further down.

## Requirements

coming soon! 

<!-- tested-with:
[gcc](http://gcc.gnu.org/)-12.2.0
[clang](https://clang.llvm.org/)-11.0.1

debian: liblo-dev libncurses-dev (c.f. r-common/README)
-->

## Building

To build type:

> make prefix=~/opt all install

## History

This tool is based on the `rju-record` utility that is included in the [`rju` tools](https://rohandrape.net/?t=rju) (formerly known as `jack-tools`) created by Rohan Drape. Since I had no need for the other tools in the package and I made a significant number of changes to `rju-record` in order for it to work the way I needed it to, I decided to fork that one utility into its own project. Now, to make sure that I give proper credit, the original copyright and contribution notices from the `rju-tools` project are here:

© [rohan drape](http://rohandrape.net/),
  2003-2023,
  [gpl](http://gnu.org/copyleft/).
  with contributions by:

- Nikita Zlobin, <!-- cook60020tmp@mail.ru --> 2016-05-14
- Yoran Heling, <!-- projects@yorhel.nl --> 2010-10-30
- Jeremy Hall, <!-- jhall@maoz.com --> 2010-09-07
- Tristan Matthews, <!-- le.businessman@gmail.com --> 2009-10-10
- Ignacio Sánchez, <!-- ignacio.sanchez@vocali.net --> 2009-02-20
- Karsten Gebbert, <!-- k.gebbert@gmail.com --> 2008-06-27

For the history of `rju-tools`, see the [history](https://rohandrape.net/?t=rju&q=history) page on Rohan's site.



## License

FoxRecorder - A CLI multitrack audio recorder for live production \
Copyright (C) 2024       Steve Cross (flip@foxhollow.cc)

rju-tools - A collection of tools for the JACK audio server \
Copyright (C) 2003-2023  Rohan Drape (rd@rohandrape.net) \
Copyright (C) 2016       Nikita Zlobin (cook60020tmp@mail.ru) \
Copyright (C) 2010       Yoran Heling (projects@yorhel.nl) \
Copyright (C) 2010       Jeremy Hall (jhall@maoz.com) \
Copyright (C) 2009       Tristan Matthews (le.businessman@gmail.com) \
Copyright (C) 2009       Ignacio Sánchez (ignacio.sanchez@vocali.net) \
Copyright (C) 2008       Karsten Gebbert (k.gebbert@gmail.com)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

