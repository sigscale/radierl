## configure.ac
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([radius_example], [1.1], [vances@sigscale.org])
AC_COPYRIGHT([Copyright (c) 2011-2024 SigScale Global Inc])
AC_PREREQ(2.64)
AC_LANG([Erlang])
AC_CONFIG_SRCDIR([src/radius_example_app.erl])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--disable-debug], [disable debugging (default is enabled)])])

##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v -pa ../../ebin"
fi
if test "x$enable_debug" != xno ; then
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
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
AC_SUBST(appdir)
AC_SUBST(beamdir)


##
## Libraries:  check for libraries
##
AC_ERLANG_CHECK_LIB([radius], [], [AC_MSG_ERROR([radius was not found!])])

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
AC_OUTPUT([Makefile ebin/Makefile doc/Makefile test/Makefile])

