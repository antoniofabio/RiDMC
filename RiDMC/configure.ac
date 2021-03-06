# largely copied/adapted from the gsl R package
AC_INIT([src/ridmc.c])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

# Following lines kindly supplied by Dirk Eddelbuettel
## Use gsl-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: gsl-config(1)
AC_PATH_PROG([GSL_CONFIG], [gsl-config])
## If gsl-config was found, let's use it
if test "${GSL_CONFIG}" != ""; then
   # Use gsl-config for header and linker arguments
   GSL_CFLAGS=`${GSL_CONFIG} --cflags`
   GSL_LIBS=`${GSL_CONFIG} --libs`
else
   AC_MSG_ERROR([gsl-config not found, is GSL installed?])
fi

AC_MSG_CHECKING([if GSL version >= 1.12])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <string.h>
#include <gsl/gsl_version.h>
int main() {
#ifdef GSL_VERSION
   int major, minor;
   char *gslv = GSL_VERSION;
   if ((sscanf(gslv, "%d.%d", &major, &minor)) != 2) {
     exit (1);
   }
   exit (minor < 12);
#else
  exit(1);
#endif
}
]])],
[gsl_version_ok=yes],
[gsl_version_ok=no],
[gsl_version_ok=yes])
if test "${gsl_version_ok}" = no; then
  AC_MSG_ERROR([Need GSL version >= 1.12])
else
  AC_MSG_RESULT([yes])
fi

PKG_PROG_PKG_CONFIG

PKG_CHECK_MODULES([LUA], [lua > 5.1 lua < 5.2],,
   [PKG_CHECK_MODULES([LUA], [lua5.1],,)])

CPPFLAGS="$CPPFLAGS $LUA_CFLAGS"
LDFLAGS="$LDFLAGS $LUA_LIBS"
AC_CHECK_HEADERS([lua.h])
AC_MSG_CHECKING([if Lua version = 5.1.x])
AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <lua.h>
int main() {
#ifdef LUA_VERSION_NUM
  if (LUA_VERSION_NUM == 501) {
    exit(0);
  } else {
    exit(1);
  }
#else
  exit(1);
#endif
}
]])],
[lua_version_ok=yes],
[lua_version_ok=no],
[lua_version_ok=yes])
if test "${lua_version_ok}" = no; then
  AC_MSG_ERROR([Need Lua version 5.1])
else
  AC_MSG_RESULT([yes])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)
AC_SUBST(LUA_CFLAGS)
AC_SUBST(LUA_LIBS)
AC_OUTPUT(src/Makevars)
