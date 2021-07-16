all:
	echo "rju"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -fR dist dist-newstyle *~
	(cd cmd ; make clean)

push-all:
	r.gitlab-push.sh rju

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
