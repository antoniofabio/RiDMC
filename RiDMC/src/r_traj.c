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
#include <stdlib.h>
#include <gsl/gsl_odeiv.h>
#include <idmclib/model.h>
#include <idmclib/traj.h>

#include "ridmc.h"

void ridmc_ctrajectory_free(SEXP p);

SEXP ridmc_ctrajectory_alloc(SEXP m, SEXP pars, SEXP vars, SEXP step_size, SEXP step_function_code) {
	idmc_traj_ctrajectory *pans;
	SEXP ans;
	static const gsl_odeiv_step_type *step_funs[10];
	step_funs[0] = gsl_odeiv_step_rk2;
	step_funs[1] = gsl_odeiv_step_rk4;
	step_funs[2] = gsl_odeiv_step_rkf45;
	step_funs[3] = gsl_odeiv_step_rkck;
	step_funs[4] = gsl_odeiv_step_rk8pd;
	step_funs[5] = gsl_odeiv_step_rk2imp;
	step_funs[6] = gsl_odeiv_step_rk4imp;
	step_funs[7] = gsl_odeiv_step_gear1;
	step_funs[8] = gsl_odeiv_step_gear2;
	int istep_fun = INTEGER(step_function_code)[0];
	int ians = idmc_traj_ctrajectory_alloc(
		R_ExternalPtrAddr(m), 
		REAL( pars ), REAL (vars),
		REAL( step_size )[0], 
		(gsl_odeiv_step_type *) step_funs[istep_fun],
		&pans);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	PDEBUG("allocated trajectory %p\n", pans);
	PROTECT(ans = R_MakeExternalPtr(pans, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_ctrajectory_free);
	UNPROTECT(1);
	return ans;
}

void ridmc_ctrajectory_free(SEXP p) {
	idmc_traj_ctrajectory_free( R_ExternalPtrAddr(p) );
	PDEBUG("deallocated trajectory %p\n", R_ExternalPtrAddr(p));
}

SEXP ridmc_ctrajectory_step(SEXP t) {
	idmc_traj_ctrajectory_step( R_ExternalPtrAddr(t) );
	return R_NilValue;
}

SEXP ridmc_ctrajectory_getValue(SEXP t) {
	idmc_traj_ctrajectory *tr = R_ExternalPtrAddr(t);
	SEXP ans = pdToNumVec(tr->var, tr->model->var_len);
	UNPROTECT(1);
	return ans;
}

SEXP ridmc_ctrajectory_getModel(SEXP t) {
	SEXP ans;
	idmc_model *m = ((idmc_traj_ctrajectory*) R_ExternalPtrAddr(t))->model;
	PROTECT(ans = R_MakeExternalPtr(m, R_NilValue, R_NilValue));
	UNPROTECT(1);
	return ans;
}

void ridmc_dtrajectory_free(SEXP p);

SEXP ridmc_dtrajectory_alloc(SEXP m, SEXP pars, SEXP vars) {
	idmc_traj_trajectory *pans;
	SEXP ans;
	int ians = idmc_traj_trajectory_alloc(
		R_ExternalPtrAddr(m), 
		REAL( pars ), REAL (vars),
		&pans);
	if(ians!=IDMC_OK)
		RIDMC_ERROR(ians);
	PDEBUG("allocated trajectory %p\n", pans);
	PROTECT(ans = R_MakeExternalPtr(pans, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_dtrajectory_free);
	UNPROTECT(1);
	return ans;
}

void ridmc_dtrajectory_free(SEXP p) {
	idmc_traj_trajectory_free( R_ExternalPtrAddr(p) );
	PDEBUG("deallocated trajectory %p\n", R_ExternalPtrAddr(p));
}

SEXP ridmc_dtrajectory_step(SEXP t) {
	idmc_traj_trajectory_step( R_ExternalPtrAddr(t) );
	return R_NilValue;
}

SEXP ridmc_dtrajectory_getValue(SEXP t) {
	idmc_traj_trajectory *tr = R_ExternalPtrAddr(t);
	SEXP ans = pdToNumVec(tr->var, tr->model->var_len);
	UNPROTECT(1);
	return ans;
}

SEXP ridmc_dtrajectory_getModel(SEXP t) {
	SEXP ans;
	idmc_model *m = ((idmc_traj_trajectory*) R_ExternalPtrAddr(t))->model;
	PROTECT(ans = R_MakeExternalPtr(m, R_NilValue, R_NilValue));
	UNPROTECT(1);
	return ans;
}
