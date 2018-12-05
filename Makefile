all:
	(cd cmd ; make all)

install:
	(cd cmd ; make install)

clean:
	(cd cmd ; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/rju

pull-rd:
	darcs pull -a http://rohandrape.net/sw/rju

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
