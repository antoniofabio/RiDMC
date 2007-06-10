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

Last modified: $Date: 2007-06-08 19:02:35 +0200 (ven, 08 giu 2007) $
*/
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdlib.h>
#include <gsl/gsl_odeiv.h>
#include <idmclib/model.h>
#include <idmclib/traj.h>

#include "ridmc.h"

SEXP ridmc_bifurcation(SEXP m, SEXP whichVar,
	SEXP par, SEXP var, SEXP whichPar,
	SEXP parValues, SEXP transient, SEXP maxPeriod) {
	SEXP ans;
	int np, mp, tr, i, j, ij, varlen;
	double *tv;
	np = length(parValues);
	varlen = length(var);
	mp = INTEGER(maxPeriod)[0];
	tr = INTEGER(transient)[0];
	idmc_model *model;
	model = (idmc_model*) R_ExternalPtrAddr(m);
	PROTECT( ans = allocVector(REALSXP, np * mp ) );
	tv = (double*) R_alloc(length(var), sizeof(double));
	for(i=0, ij=0; i<np; i++) {
		R_CheckUserInterrupt();
		REAL(par)[INTEGER(whichPar)[0]] = REAL(parValues)[i];
		memcpy(tv, REAL(var), length(var)*sizeof(double));
		for(j=0; j<tr; j++)
			idmc_model_f(model, REAL(par), tv, tv);
		for(j=0; j<mp; j++, ij++) {
			idmc_model_f(model, REAL(par), tv, tv);
			REAL(ans)[ij] = tv[INTEGER(whichVar)[0]];
		}
	}
	UNPROTECT(1);
	return ans;
}
