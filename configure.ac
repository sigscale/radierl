## configure.ac
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([radius], [1.0], [vances@motivity.ca])
AC_COPYRIGHT([Copyright (c) 2011 Motivity Telecom])
AC_PREREQ(2.64)
AC_LANG([Erlang])
AC_CONFIG_SRCDIR([src/radius_app.erl])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--enable-debug], [enable debugging (default is no)])])

##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v"
fi
if test "x$enable_debug" == xyes ; then
	ERLCFLAGS="$ERLCFLAGS +debug_info"
fi
AC_ERLANG_SUBST_LIB_DIR
if test -z "${ERLANG_INSTALL_LIB_DIR}" ; then
	ERLANG_INSTALL_LIB_DIR=$ERLANG_LIB_DIR
fi
AC_PROG_INSTALL
AC_ERLANG_SUBST_INSTALL_LIB_DIR
appdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION
beamdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
eincludedir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/include
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
AC_SUBST(appdir)
AC_SUBST(beamdir)
AC_SUBST(eincludedir)


##
## Libraries:  check for libraries
##


##
## Headers:  check for header files
##


##
## Typedefs & Structures:  check for typedefs, structures,
##                         and compiler characteristics.
##


##
## Functions:  check for library functions
##


##
## Output:  create output files
## 
AC_OUTPUT([Makefile include/Makefile ebin/Makefile doc/Makefile])

