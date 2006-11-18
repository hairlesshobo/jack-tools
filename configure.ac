# configure.ac                            -*-autoconf-*-

# Initialize autoconf and autmake

AC_INIT(jack.tools,0.1)
AC_CONFIG_AUX_DIR(config)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE

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

AC_CHECK_LIB(jack, 
             jack_client_new, 
	     , 
	     AC_MSG_ERROR("No JACK library.")
	     ,)
AC_CHECK_HEADERS(jack/jack.h)

# Require LIBSNDFILE sound file format library.

AC_CHECK_LIB(sndfile, 
             sf_open, 
	     , 
	     AC_MSG_ERROR("No LibSndFile library.")
	     ,)
AC_CHECK_HEADERS(sndfile.h)

# Require X11

AC_PATH_XTRA
if test "$no_x" = yes; then
   AC_MSG_ERROR("jack.scope requires the X11 libraries")
fi
CFLAGS="$CFLAGS $X_CFLAGS"
LIBS="$LIBS $X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS -lX11 -lXext -lm"

# Set compiler flags.

AC_SUBST(AM_CFLAGS)
AM_CFLAGS="-O3 -funroll-loops -Wall"

# Write Makefile.

AC_OUTPUT(Makefile common/Makefile)
