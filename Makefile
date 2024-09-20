all:
	(cd src ; make all)

clean:
	(cd src ; make clean)

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
