MAINTAINERCLEANFILES =	aclocal.m4	\
			config.guess	\
			config.log	\
			config.status	\
			config.sub	\
			configure	\
			depcomp		\
			install-sh	\
			ltmain.sh	\
			Makefile.in	\
			missing		\
			mkinstalldirs

SUBDIRS = common

bin_PROGRAMS =	jack.osc \
		jack.play \
		jack.plumbing \
		jack.record \
		jack.scope \
		jack.transport \
		jack.udp

jack_osc_SOURCES = jack.osc.c
jack_osc_LDADD = common/libcommon.a

jack_play_SOURCES = jack.play.c
jack_play_LDADD = common/libcommon.a

jack_plumbing_SOURCES = jack.plumbing.c
jack_plumbing_LDADD = common/libcommon.a

jack_record_SOURCES = jack.record.c
jack_record_LDADD = common/libcommon.a

jack_scope_SOURCES = jack.scope.c
jack_scope_LDADD = common/libcommon.a

jack_transport_SOURCES = jack.transport.c
jack_transport_LDADD = common/libcommon.a -lncurses

jack_udp_SOURCES = jack.udp.c
jack_udp_LDADD = common/libcommon.a

man_MANS =	jack.osc.1 \
		jack.play.1 \
		jack.plumbing.1 \
		jack.record.1 \
		jack.scope.1 \
		jack.transport.1 \
		jack.udp.1
EXTRA_DIST = $(man_MANS)
