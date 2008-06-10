: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_INIT([RiDMC], 0.10-0)

AC_CHECK_LIB(m, main)
AC_CHECK_LIB(gslcblas, main,, [AC_MSG_ERROR([Cannot find gslcblas library])] )
AC_CHECK_LIB(gsl, main,,  [AC_MSG_ERROR([Cannot find gsl library])] )

AC_CHECK_LIB(lua5.1, luaL_newstate,,[AC_MSG_ERROR([Cannot find lua5.1 library])])
AC_CHECK_HEADER(lua5.1/lua.h, , [AC_MSG_ERROR([Cannot find lua headers])])

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

