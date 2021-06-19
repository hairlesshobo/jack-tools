GL_GIT=git@gitlab.com:rd--/rju.git
GL_HTTP=https://gitlab.com/rd--/rju.git

all:
	echo "rju"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -fR dist dist-newstyle *~
	(cd cmd ; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

push-tags:
	git push $(GL_GIT) --tags

update-rd:
	ssh rd@rohandrape.net "(cd sw/rju ; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
