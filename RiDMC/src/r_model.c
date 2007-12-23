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

void ridmc_model_free(SEXP p);

SEXP ridmc_model_alloc(SEXP in_buf) {
	SEXP sxp_buf;
	SEXP ans;
	char *buf;
	char msg[IDMC_MAXSTRLEN];
	int buflen, ians;
	idmc_model *model;
	PROTECT( sxp_buf = coerceVector( in_buf, STRSXP ) );
	buf = (char*) CHAR( STRING_ELT(sxp_buf,0) );
	buflen = strlen(buf);
	ians = idmc_model_alloc(buf, buflen, &model);
	UNPROTECT(1);
	if(ians == IDMC_ELUASYNTAX) {
		strcpy(msg, model->errorMessage);
		idmc_model_free(model);
		error("[idmclib error: %s] %s\n", idmc_err_message[ians], msg);
	} else if(ians != IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	PDEBUG("allocated model %p\n", model);
	PROTECT(ans = R_MakeExternalPtr(model, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_model_free);
	UNPROTECT(1);
	return ans;
}

void ridmc_model_free(SEXP p) {
	PDEBUG("deallocating model %p\n", R_ExternalPtrAddr(p));
	idmc_model_free( R_ExternalPtrAddr(p) );
}

SEXP ridmc_model_clone(SEXP m) {
	SEXP ans;
	idmc_model* pans = idmc_model_clone(R_ExternalPtrAddr(m));
	PROTECT(ans = R_MakeExternalPtr(pans, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_model_free);
	UNPROTECT(1);
	return ans;
}

SEXP ridmc_model_f(SEXP m, SEXP par, SEXP var) {
	SEXP ans;
	PROTECT(ans = allocVector(REALSXP, length(var)) );
	int ians = idmc_model_f(R_ExternalPtrAddr(m), 
		REAL(par),
		REAL(var),
		REAL(ans));
	UNPROTECT(1);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}
SEXP ridmc_model_g(SEXP m, SEXP par, SEXP var) {
	SEXP ans;
	PROTECT(ans = allocVector(REALSXP, length(var)) );
	int ians = idmc_model_g(R_ExternalPtrAddr(m), 
		REAL(par),
		REAL(var),
		REAL(ans));
	UNPROTECT(1);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}

SEXP ridmc_model_Jf(SEXP m, SEXP par, SEXP var) {
	SEXP ans;
	PROTECT( ans = allocVector(REALSXP, length(var) * length(var) ) );
	int ians = idmc_model_Jf(
		R_ExternalPtrAddr(m),
		REAL(par),
		REAL(var),
		REAL(ans));
	UNPROTECT(1);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}

SEXP ridmc_model_Jg(SEXP m, SEXP par, SEXP var) {
	SEXP ans;
	PROTECT( ans = allocVector(REALSXP, length(var) * length(var) ) );
	int ians = idmc_model_Jg(
		R_ExternalPtrAddr(m),
		REAL(par),
		REAL(var),
		REAL(ans));
	UNPROTECT(1);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}

SEXP ridmc_model_NumJf(SEXP m, SEXP par, SEXP var) {
	SEXP ans;
	double *util;
	int nvar = length(var);
	PROTECT( ans = allocVector(REALSXP, nvar * nvar ) );
	util = (double*) R_alloc( 3 * nvar, sizeof(double) );
	int ians = idmc_model_NumJf(
		R_ExternalPtrAddr(m),
		REAL(par),
		REAL(var),
		REAL(ans),
		util, util+nvar, util + (2*nvar));
	UNPROTECT(1);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return ans;
}

SEXP ridmc_model_setGslRngSeed(SEXP m, SEXP seed) {
	int ians = idmc_model_setGslRngSeed(R_ExternalPtrAddr(m), INTEGER(seed)[0]);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	return R_NilValue;
}

/*
Get a list filled with model infos
*/
SEXP ridmc_model_getInfos(SEXP m) {
	SEXP ans, strings, flags, lens, parNames, varNames;
	int i;
	idmc_model *pm = (idmc_model*) R_ExternalPtrAddr(m);
	PROTECT(ans = allocVector(VECSXP, 5));
	PROTECT(strings = allocVector(STRSXP, 3));
	PROTECT(flags = allocVector(INTSXP, 2));
	PROTECT(lens = allocVector(INTSXP, 2) );
	SET_STRING_ELT( strings, 0, mkChar(pm->name));
	SET_STRING_ELT( strings, 1, mkChar(pm->desc));
	SET_STRING_ELT( strings, 2, mkChar(pm->type));
	INTEGER(flags)[0] = pm->has_inverse;
	INTEGER(flags)[1] = pm->has_jacobian;
	INTEGER(lens)[0] = pm->par_len;
	INTEGER(lens)[1] = pm->var_len;
	PROTECT(parNames = allocVector(STRSXP, pm->par_len) );
	for(i=0; i<pm->par_len; i++)
		SET_STRING_ELT( parNames, i, mkChar(pm->par[i]));
	PROTECT(varNames = allocVector(STRSXP, pm->var_len) );
	for(i=0; i<pm->var_len; i++)
		SET_STRING_ELT( varNames, i, mkChar(pm->var[i]));
	SET_VECTOR_ELT(ans, 0, strings);
	SET_VECTOR_ELT(ans, 1, flags);
	SET_VECTOR_ELT(ans, 2, lens);
	SET_VECTOR_ELT(ans, 3, parNames);
	SET_VECTOR_ELT(ans, 4, varNames);
	UNPROTECT(6);
	return ans;
}
