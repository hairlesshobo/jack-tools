all:
	echo "rju"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -fR dist
	(cd cmd ; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/rju

pull-rd:
	darcs pull -a http://rohandrape.net/sw/rju

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
