/*
ridmc: iDMC->R interface

Copyright (C) 2008 Marji Lines and Alfredo Medio.

Written by Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or any
later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
*/
#include "ridmc.h"
#include <idmclib/model.h>
#include <idmclib/attractor.h>
#include <idmclib/basin_multi.h>

void ridmc_basin_multi_free(SEXP p);

/* Allocate new 'idmc_basin_slow' object
m: model object (object is cloned)
parameters: model parameters (vector will be copied)
xmin, xmax, xres: x axis ranges and resolution
ymin, ymax, yres: y axis ranges and resolution
eps: neighborhood treshold to be used for attractor identification
attractorLimit: limit number of iterations before encountering an attractor
attractorIterations: limit number of iterations for drawing an attractor
xvar, yvar: variables to put on x and y axes, respectively
startValues: vector of starting values. 'xvar' and 'yvar' elmts will be ignored.
returns: allocated basin object
*/
SEXP ridmc_basin_multi_alloc(SEXP model, SEXP parms,
	SEXP xmin, SEXP xmax, SEXP xres,
	SEXP ymin, SEXP ymax, SEXP yres,
	SEXP eps, SEXP attractorLimit, SEXP attractorIterations, SEXP ntries,
	SEXP xvar, SEXP yvar, SEXP startValues) {
	SEXP ans;
	int ians;
	idmc_basin_multi *basin;

	ians = idmc_basin_multi_alloc(R_ExternalPtrAddr(model), REAL(parms),
		REAL(xmin)[0], REAL(xmax)[0], INTEGER(xres)[0],
		REAL(ymin)[0], REAL(ymax)[0], INTEGER(yres)[0],
		REAL(eps)[0], INTEGER(attractorLimit)[0], INTEGER(attractorIterations)[0],
		INTEGER(ntries)[0], INTEGER(xvar)[0], INTEGER(yvar)[0], REAL(startValues), &basin);
	if(ians != IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	PDEBUG("allocated basin %p\n", basin);
	PROTECT(ans = R_MakeExternalPtr(basin, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_basin_multi_free);
	UNPROTECT(1);
	return ans;
}

/*deallocates an idmc_basin_multi object*/
void ridmc_basin_multi_free(SEXP p) {
	PDEBUG("deallocating basin %p\n", R_ExternalPtrAddr(p));
	idmc_basin_multi_free( R_ExternalPtrAddr(p) );
}

SEXP ridmc_basin_multi_find_next_attractor(SEXP p) {
	int ians = idmc_basin_multi_find_next_attractor(R_ExternalPtrAddr(p));
	if(ians!=IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	return R_NilValue;
}

SEXP ridmc_basin_multi_step(SEXP p) {
	int ians = idmc_basin_multi_step(R_ExternalPtrAddr(p));
	if(ians!=IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	return R_NilValue;
}

SEXP ridmc_basin_multi_finished(SEXP p) {
	SEXP ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = idmc_basin_multi_finished(R_ExternalPtrAddr(p));
	return ans;
}

SEXP ridmc_basin_multi_setGslRngSeed(SEXP b, SEXP seed) {
	idmc_model_setGslRngSeed(((idmc_basin_multi*)R_ExternalPtrAddr(b))->model,
		INTEGER(seed)[0]);
	return R_NilValue;
}

SEXP ridmc_basin_multi_getData(SEXP p) {
	SEXP ans;
	idmc_basin_multi *bp = (idmc_basin_multi*) R_ExternalPtrAddr(p);
	ans = allocMatrix(INTSXP, bp->raster->xres,  bp->raster->yres );
	memcpy(INTEGER(ans), bp->raster->data, bp->dataLength * sizeof(int));
	return ans;
}

SEXP ridmc_basin_multi_get_attractors(SEXP p) {
	SEXP ans; int i, j, k;
	idmc_basin_multi *bp = (idmc_basin_multi*) R_ExternalPtrAddr(p);
	idmc_attractor* attr_head = bp->attr_head;
	idmc_attractor* tmpa = 0;
	idmc_attractor_point* tmpp = 0;
	int ntmp;
	int nattr = attr_head ? idmc_attractor_list_length(attr_head) : 0;
	int dim = bp->model->var_len;
	PROTECT(ans = allocVector(VECSXP, nattr));
	for(i=0; i<nattr; i++) {
		tmpa = idmc_attractor_list_get(attr_head, i);
		ntmp = idmc_attractor_length(tmpa);
		SET_VECTOR_ELT(ans, i, allocMatrix(REALSXP, ntmp, dim));
		tmpp = tmpa->hd;
		j=0;
		while(tmpp) {
			for(k=0; k<dim; k++)
				REAL(VECTOR_ELT(ans, i))[j + k*ntmp] = tmpp->x[k];
			j++;
			tmpp = tmpp->next;
		}
	}
	UNPROTECT(1);
	return ans;
}
