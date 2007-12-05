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
#include <R.h>
#include <Rinternals.h>
#include <idmclib/model.h>
#include <idmclib/lexp.h>

#include "ridmc.h"

SEXP ridmc_lexp(SEXP m, SEXP par, SEXP startPoint, SEXP iterations) {
	idmc_model *pm = R_ExternalPtrAddr(m);
	SEXP ans, trashStartPoint;
	PROTECT(ans = allocVector(REALSXP, pm->var_len));
        PROTECT(trashStartPoint = allocVector(REALSXP, pm->var_len));
        for(int i=0; i<pm->var_len; i++)
          REAL(trashStartPoint)[i] = REAL(startPoint)[i];
	int ians = idmc_lexp(pm,
		REAL(par),
		REAL(trashStartPoint),
		REAL(ans),
		INTEGER(iterations)[0]);
	UNPROTECT(2);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}

SEXP ridmc_lexp_ode(SEXP m, SEXP par, SEXP startPoint, SEXP time, SEXP step) {
	idmc_model *pm = R_ExternalPtrAddr(m);
	SEXP ans, trashStartPoint;
	PROTECT(ans = allocVector(REALSXP, pm->var_len));
        PROTECT(trashStartPoint = allocVector(REALSXP, pm->var_len));
        for(int i=0; i<pm->var_len; i++)
          REAL(trashStartPoint)[i] = REAL(startPoint)[i];
	int ians = idmc_lexp_ode(pm,
		REAL(par),
		REAL(trashStartPoint),
		REAL(ans),
		REAL(time)[0], REAL(step)[0]);
	UNPROTECT(2);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}


/*int idmc_lexp_ode_step(idmc_model* model, const double *parameters, double* result,
						  double* Q, double* y, double* t, double step);
*/
