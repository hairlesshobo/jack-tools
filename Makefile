push-sp:
	darcs push -a rd@slavepianos.org:sw/rju

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/rju

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
