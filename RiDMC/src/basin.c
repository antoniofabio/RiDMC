/*
iDMC C library

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

Last modified: $Date$
*/
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "defines.h"
#include "raster.h"
#include "model.h"
#include "basin.h"

int idmc_basin_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres,
	int attractorLimit, int attractorIterations,
	idmc_basin** out_basin)
{
	int i;
	idmc_basin* ans;
	idmc_raster* raster;
	ans = (idmc_basin*) malloc( sizeof(idmc_basin) );
	if(ans==NULL)
		return IDMC_EMEM;
	ans->model = idmc_model_clone(m);
	if(ans->model==NULL) {
		idmc_basin_free(ans);
		return IDMC_EMEM;
	}
	ans->parameters = (double*) malloc( m->par_len * sizeof(double));
	if(ans->parameters==NULL) {
		idmc_basin_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->parameters, parameters, m->par_len * sizeof(double));
	i = idmc_raster_alloc(xmin, xmax, xres, ymin, ymax, yres, &raster);
	if(i != IDMC_OK) {
		idmc_basin_free(ans);
		return i;
	}
	idmc_raster_set(raster, 0);
	ans->raster = raster;

	ans->currentPoint = (double*) malloc(2*sizeof(double));
	if(ans->currentPoint==NULL) {
		idmc_basin_free(ans);
		return IDMC_EMEM;
	}
	ans->startPoint = (double*) malloc(2*sizeof(double));
	if(ans->startPoint==NULL) {
		idmc_basin_free(ans);
		return IDMC_EMEM;
	}
	ans->work = (double*) malloc(2*sizeof(double));
	if(ans->work==NULL) {
		idmc_basin_free(ans);
		return IDMC_EMEM;
	}

	ans->dataLength = ans->raster->xres * ans->raster->yres;
	ans->attractorLimit = attractorLimit;
	ans->attractorIterations = attractorIterations;

	ans->currId = 0;
        ans->attractorColor = 2;
        ans->basinColor = 3;
	ans->index = 0;

	*out_basin = ans;
	return IDMC_OK;
}

void idmc_basin_free(idmc_basin* p) {
	if(p->model!=NULL)
		idmc_model_free(MODEL(p));
	if(p->parameters!=NULL)
		free(p->parameters);
	if(p->raster!=NULL)
		idmc_raster_free(RASTER(p));
	if(p->currentPoint!=NULL)
		free(p->currentPoint);
	if(p->startPoint!=NULL)
		free(p->startPoint);
	if(p->work!=NULL)
		free(p->work);
	free(p);
}

static void fillBasinTrack(idmc_basin* p, double *startPoint, int iterations, int value);

/*
Iterates one cell in the basin grid. Algorithm due to Daniele Pizzoni, 
translated from the iDMC (1.9.4 and following versions) java software
*/
/*some utility definitions:*/
#define attractorLimit (p->attractorLimit)
#define attractorIterations (p->attractorIterations)
#define startPoint (p->startPoint)
#define currentPoint (p->currentPoint)
#define index (p->index)
#define state (p->state)
#define attr (p->attr)
#define color (p->color)
int idmc_basin_step(idmc_basin* p) {
	if( basin_finished(p) ) /*algorithm ended*/
		return IDMC_OK;

	getCurrPoint(p, startPoint); /*get start point coordinates*/
	index++; /*iteration index*/
	memcpy(currentPoint, startPoint, 2 * sizeof(double) ); /*copy start point to current point*/
	setValue(p, currentPoint, p->basinColor); /*set cell value to current basin color*/
	
	color = p->basinColor;
	attr = -1;

	for (int i = 0;;) {
		idmc_model_f(MODEL(p), p->parameters, currentPoint, currentPoint);
		i++;
		if (isPointInfinite(currentPoint)) {
			fillBasinTrack(p, startPoint, i, IDMC_BASIN_INFINITY);
			break;
		}
		if (!isPointInsideBounds(p, currentPoint)) {
			if (i >= attractorLimit) {
				fillBasinTrack(p, startPoint, i, IDMC_BASIN_INFINITY);
				break;
			}
			else
				continue;
		}

		state = getValue(p, currentPoint);
		
		/* untouched */
		if (state == 0) {
			setValue(p, currentPoint, color);
			continue;
		}
		/* infinity */
		if (state == IDMC_BASIN_INFINITY) {
			fillBasinTrack(p, startPoint, i - 1, IDMC_BASIN_INFINITY);
			break;
		}

		/*current basin color*/
		if (state == p->basinColor) {
			if (attr == -1) {
				attr = i;
				continue;
			}
			else if (i - attr < attractorLimit) {
				continue;
			}
			else if (i - attr < (attractorLimit+attractorIterations)) {
				if (color != p->attractorColor) {
					color = p->attractorColor;
				}
		
				setValue(p, currentPoint, color);
				continue;
			}
			else {
				continue;
			}
			// not reached
		}

		/*current attractor color*/
		if (state == p->attractorColor) {
			if ( (attr != -1) && ( (i - attr) < (attractorLimit+attractorIterations) ) ) {
				continue;
			}
			else {
				p->attractorColor += 2;
				p->basinColor = p->attractorColor+1;
				//addSamplePoint(p, currentPoint);
				break;
			}
		}
		attr = -1;
		
		/* another basin encountered */
		if (!isOdd(state) && 
			( state != p->basinColor ) && 
			( state != IDMC_BASIN_INFINITY ) ) {
				fillBasinTrack(p, startPoint, i - 1, state);
			break;
		}
		
		/* another attractor encountered */
		if (isOdd(state) && 
			(state != p->attractorColor) && 
			(state != IDMC_BASIN_INFINITY) ) {
				fillBasinTrack(p, startPoint, i - 1, state+1);
			break;
		}

		/*shouldn't be reached*/
		return IDMC_EMATH;
	}
	return IDMC_OK;
}
#undef attractorLimit
#undef attractorIterations
#undef startPoint
#undef currentPoint
#undef index
#undef state
#undef attr
#undef color


static void fillBasinTrack(idmc_basin* p, double *startPoint, int iterations, int value) {
	memcpy(p->work, startPoint, 2 * sizeof(double));
	setValue(p, p->work, value);
	for(int i=0; i<iterations; i++) {
		idmc_model_f(MODEL(p), p->parameters, p->work, p->work);
		if(!isPointInsideBounds(p, p->work))
			continue;
		setValue(p, p->work, value);
	}
}

int idmc_basin_finished(idmc_basin *p) {
	return basin_finished(p);
}
