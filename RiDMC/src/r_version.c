/*
ridmc: iDMC->R interface

Copyright (C) 2007,2009 Marji Lines and Alfredo Medio.

Written by Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or any
later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Last modified: $Date: 2007-05-24 16:41:18 +0200 (gio, 24 mag 2007) $
*/
#include "ridmc.h"
#include <idmclib/version.h>

/*Return idmclib version number as (major, minor, micro) vector*/
SEXP ridmc_version() {
  SEXP ans, dimnames;
  PROTECT(ans = allocVector(INTSXP, 3));
  INTEGER(ans)[0] = idmc_version_major();
  INTEGER(ans)[1] = idmc_version_minor();
  INTEGER(ans)[2] = idmc_version_micro();
  PROTECT(dimnames = allocVector(STRSXP, 3));
  SET_STRING_ELT(dimnames, 0, mkChar("major"));
  SET_STRING_ELT(dimnames, 1, mkChar("minor"));
  SET_STRING_ELT(dimnames, 2, mkChar("micro"));
  setAttrib(ans, R_NamesSymbol, dimnames);
  UNPROTECT(2);
  return ans;
}
