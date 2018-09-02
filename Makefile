all:
	(cd cmd ; make all)

install:
	(cd cmd ; make install)

clean:
	(cd cmd ; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/rju

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/rju

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
