: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_INIT([RiDMC], 0.8.0)

AC_CHECK_LIB(m, main)
AC_CHECK_LIB(gslcblas, main,, [AC_MSG_ERROR([Cannot find gslcblas library])] )
AC_CHECK_LIB(gsl, main,,  [AC_MSG_ERROR([Cannot find gsl library])] )

AC_CHECK_LIB(lua50, lua_open,,[AC_MSG_ERROR([Cannot find lua50 library])])
AC_CHECK_LIB(lualib50, main,, [AC_MSG_ERROR([Cannot find lualib50 library])] )
AC_CHECK_HEADER(lua50/lua.h, , [AC_MSG_ERROR([Cannot find lua headers])])

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

