/*
ridmc: iDMC->R interface

Copyright (C) 2007 Marji Lines and Alfredo Medio.

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
#include <idmclib/model.h>
#include <idmclib/cycles.h>

SEXP ridmc_cycles_find(SEXP m, SEXP parms, SEXP start_point, SEXP power, SEXP epsilon,
		       SEXP max_iterations) {
  idmc_model *pm = R_ExternalPtrAddr(m);
  SEXP result, eigvals, ans, dimnames;
  PROTECT(result = allocVector(REALSXP, pm->var_len));
  PROTECT(eigvals = allocVector(REALSXP, pm->var_len));
  int ians = idmc_cycles_find(pm, REAL(parms), REAL(start_point),
			      INTEGER(power)[0], REAL(epsilon)[0],
			      INTEGER(max_iterations)[0],
			      REAL(result), REAL(eigvals));
  if(ians!=IDMC_OK) {
    UNPROTECT(2);
    RIDMC_ERROR(ians);
  }
  PROTECT(ans = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, result);
  SET_VECTOR_ELT(ans, 1, eigvals);
  PROTECT(dimnames = allocVector(STRSXP, 2));
  SET_STRING_ELT(dimnames, 0, mkChar("result"));
  SET_STRING_ELT(dimnames, 1, mkChar("eigvals"));
  setAttrib(ans, R_NamesSymbol, dimnames);
  UNPROTECT(4);
  return ans;
}
