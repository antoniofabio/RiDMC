: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_INIT([RiDMC], 0.10-10)

AC_CHECK_LIB(m, main)
AC_CHECK_LIB(gslcblas, main,, [AC_MSG_ERROR([Cannot find gslcblas library])] )
AC_CHECK_LIB(gsl, main,,  [AC_MSG_ERROR([Cannot find gsl library])] )
AC_SEARCH_LIBS(luaL_newstate, lua lua5.1,,[AC_MSG_ERROR([Cannot find lua library])])

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
