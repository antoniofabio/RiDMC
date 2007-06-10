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
#include <idmclib/basin.h>

void ridmc_basin_free(SEXP p);

SEXP ridmc_basin_alloc(SEXP model, SEXP parms,
	SEXP xmin, SEXP xmax, SEXP xres,
	SEXP ymin, SEXP ymax, SEXP yres, 
	SEXP attractorLimit, SEXP attractorIterations) {
	SEXP ans;
	int ians;
	idmc_basin *basin;
	
	ians = idmc_basin_alloc(R_ExternalPtrAddr(model), REAL(parms), 
		REAL(xmin)[0], REAL(xmax)[0], INTEGER(xres)[0],
		REAL(ymin)[0], REAL(ymax)[0], INTEGER(yres)[0],
		INTEGER(attractorLimit)[0], INTEGER(attractorIterations)[0],
		&basin);
	if(ians != IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	PDEBUG("allocated basin %p\n", basin);
	PROTECT(ans = R_MakeExternalPtr(basin, R_NilValue, R_NilValue));
	R_RegisterCFinalizer(ans, ridmc_basin_free);
	UNPROTECT(1);
	return ans;
}

void ridmc_basin_free(SEXP p) {
	PDEBUG("deallocating basin %p\n", R_ExternalPtrAddr(p));
	idmc_basin_free( R_ExternalPtrAddr(p) );
}

SEXP ridmc_basin_step(SEXP p) {
	int ians = idmc_basin_step(R_ExternalPtrAddr(p));
	if(ians!=IDMC_OK)
		RIDMC_GENERIC_ERROR(ians);
	return R_NilValue;
}

SEXP ridmc_basin_finished(SEXP p) {
	SEXP ans = allocVector(INTSXP, 1);
	INTEGER(ans)[0] = idmc_basin_finished(R_ExternalPtrAddr(p));
	return ans;
}

SEXP ridmc_basin_getData(SEXP p) {
	SEXP ans;
	idmc_basin *bp = (idmc_basin*) R_ExternalPtrAddr(p);
	ans = allocMatrix(INTSXP, bp->raster->xres,  bp->raster->yres );
	memcpy(INTEGER(ans), bp->raster->data, bp->dataLength * sizeof(int));
	return ans;
}
