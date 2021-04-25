GL_GIT=git@gitlab.com:rd--/rju.git
GL_HTTP=https://gitlab.com/rd--/rju.git

all:
	echo "rju"

mk-cmd:
	(cd cmd ; make all install)

clean:
	rm -fR dist
	(cd cmd ; make clean)

push-gl:
	git push $(GL_GIT)

push-gl-tags:
	git push $(GH_GIT) --tag

pull-gl:
	git pull $(GL_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/rju ; git pull $(GL_HTTP))"

push-all:
	make push-gl update-rd

debian:
	sudo apt-get install liblo-dev libsamplerate0-dev
