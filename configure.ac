# configure.ac                            -*-autoconf-*-

# Initialize autoconf and autmake

AC_INIT(jack.*,0.5)
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE
PKG_PROG_PKG_CONFIG

# Check for programs. These macros set and export variables that are
# used in the make process.

AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_AWK
AC_PROG_LD
AM_PROG_LIBTOOL

# Require JACK library.

PKG_CHECK_MODULES(JACK, jack, , AC_MSG_ERROR("jack required"))

# Require sndfile library.

PKG_CHECK_MODULES(SNDFILE, sndfile, , AC_MSG_ERROR("libsndfile required"))

# Require samplerate library.

PKG_CHECK_MODULES(SRC, samplerate, , AC_MSG_ERROR("libssamplerate required"))

# Require liblo library.

PKG_CHECK_MODULES(LIBLO, liblo, , AC_MSG_ERROR("liblo required"))

# Require X11

PKG_CHECK_MODULES(X11, x11, , AC_MSG_ERROR("x11 required"))
PKG_CHECK_MODULES(XEXT, xext, , AC_MSG_ERROR("xext required"))
PKG_CHECK_MODULES(Xt, xt, , AC_MSG_ERROR("xt required"))

# Require inotify

AC_CHECK_HEADERS(sys/inotify.h, AC_DEFINE(HAVE_INOTIFY, 1, inotify))

# Set compiler flags.

AC_SUBST(AM_CFLAGS)
AM_CFLAGS="-D_POSIX_C_SOURCE=200112 -std=c99 -O3 -funroll-loops -Wall"

# Write Makefile.

AC_OUTPUT(Makefile common/Makefile)
