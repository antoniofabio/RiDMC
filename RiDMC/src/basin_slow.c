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

SLOW BASINS ALGORITHM
*/
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include "basin_common.h"
#include "defines.h"
#include "gsl_rng_lualib.h"
#include "raster.h"
#include "model.h"
#include "basin_slow.h"

#define getAttractorIndex(color) ((color)-2)/2
#define getAttractorColor(index) (index)*2+2
#define fillBasinSlowTrack(p, startPoint, iterations, value) fillRasterTrack(RASTER(p), MODEL(p), \
	(p)->parameters, startPoint, iterations, value, p->work)

int idmc_basin_slow_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres,
	int attractorLimit, int attractorIterations, int ntries,
	idmc_basin_slow** out_basin)
{
	int i;
	idmc_basin_slow* ans;
	idmc_raster* raster;
	ans = (idmc_basin_slow*) malloc( sizeof(idmc_basin_slow) );
	if(ans==NULL)
		return IDMC_EMEM;
	ans->model = idmc_model_clone(m);
	if(ans->model==NULL) {
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->parameters = (double*) malloc( m->par_len * sizeof(double));
	if(ans->parameters==NULL) {
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->parameters, parameters, m->par_len * sizeof(double));
	i = idmc_raster_alloc(xmin, xmax, xres, ymin, ymax, yres, &raster);
	if(i != IDMC_OK) {
		idmc_basin_slow_free(ans);
		return i;
	}
	idmc_raster_set(raster, 0);
	ans->raster = raster;

	ans->currentPoint = (double*) malloc(2*sizeof(double));
	if(ans->currentPoint==NULL) {
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->startPoint = (double*) malloc(2*sizeof(double));
	if(ans->startPoint==NULL) {
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->work = (double*) malloc(2*sizeof(double));
	if(ans->work==NULL) {
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->attractorsSamplePoints = (int*) malloc(ntries*sizeof(int));
	if(ans->attractorsSamplePoints == NULL){
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->attractorsCoincidence = (int*) malloc(ntries*sizeof(int));
	if(ans->attractorsCoincidence == NULL){
		idmc_basin_slow_free(ans);
		return IDMC_EMEM;
	}
	ans->nAttractors = 1;
	ans->dataLength = ans->raster->xres * ans->raster->yres;
	ans->attractorLimit = attractorLimit;
	ans->attractorIterations = attractorIterations;
	ans->ntries = ntries;
	ans->initialized = 0;
	ans->currId = 0;

	*out_basin = ans;
	return IDMC_OK;
}

void idmc_basin_slow_free(idmc_basin_slow* p) {
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
	if(p->attractorsSamplePoints!=NULL)
		free(p->attractorsSamplePoints);
	if(p->attractorsCoincidence!=NULL)
		free(p->attractorsCoincidence);
	free(p);
}

#define STEP(ans) idmc_model_f(m, p->parameters, (ans), (ans))
/*
Init basin_slow object: find attractors
*/
int idmc_basin_slow_init(idmc_basin_slow* p) {
	idmc_model *m = MODEL(p);
	idmc_raster *r = RASTER(p);
	gsl_rng* rng = getGslRngState(m->L);
	int try, xres, yres;
	int x,y;
	double xy[2];
	int i, attractorColor, attractorIndex, gs, isInfinite, isNewAttractor;
	int *attractorsCoincidence = p->attractorsCoincidence;
	xres = r->xres;
	yres = r->yres;
	p->nAttractors = 1;
	attractorIndex = 0;
	/*clean raster data*/
	memset(r->data, 0, (r->xres)*(r->yres)*sizeof(int));
	/*for each try...*/
	for(try = p->ntries; try>0; try--) {
		/*some initialization code:*/
		attractorColor = getAttractorColor(attractorIndex);
		memset(attractorsCoincidence, 0, (p->ntries)*sizeof(int));
		isInfinite=0;
		isNewAttractor=1;
		/*set random starting point*/
		x = (int)(gsl_rng_uniform(rng) * (double)xres);
		y = (int)(gsl_rng_uniform(rng) * (double)yres);
		xy[0] = idmc_raster_XY2x(r, x, y);
		xy[1] = idmc_raster_XY2x(r, x, y);
		/*start map iterations*/
		for(i = p->attractorLimit + p->attractorIterations; i>0; i--) {
			STEP(xy);
			if(isPointInfinite(xy)) {
				isInfinite=1;
				break;
			}
			if (i> (p->attractorLimit) && isPointInsideBounds(p, xy)){
				gs = getValue(p, xy);
				if (gs!=0){
					if (gs!=IDMC_BASIN_INFINITY){
						attractorColor = gs;
						attractorsCoincidence[getAttractorIndex(attractorColor)]++;
					}
				}
			}
		}/*end map iterations*/
		if (!isInfinite){
			for (int ii=0;ii<attractorIndex;ii++){
				if (attractorsCoincidence[ii]>OVERLAP_FACTOR*(p->attractorIterations)){
					isNewAttractor=0;
					break;
				}
			}
		}
		else {
			isNewAttractor=0;
		}
		if (!isInfinite){
			if (isNewAttractor) {
				fillBasinSlowTrack(p,
					xy,
					p->attractorIterations,
					getAttractorColor(attractorIndex));
				p->attractorsSamplePoints[attractorIndex] = idmc_raster_xy2I(
					r, xy[0], xy[1]);
				attractorIndex++;
			}
		}
	}/*end for each try*/
	p->nAttractors = attractorIndex-1;
	p->initialized = 1;
	return IDMC_OK;
}

/*
Iterates one cell in the basin grid. Algorithm due to A. Grigoriev,
translated from the iDMC java software
*/
/*some utility definitions:*/
#define attractorLimit (p->attractorLimit)
#define attractorIterations (p->attractorIterations)
#define startPoint (p->startPoint)
#define currentPoint (p->currentPoint)
#define state (p->state)
int idmc_basin_slow_step(idmc_basin_slow* p) {
	if( basin_finished(p) ) /*algorithm ended*/
		return IDMC_OK;
	if(!(p->initialized)) {
		return idmc_basin_slow_init(p);
	}
	idmc_model *m = MODEL(p);
	int i;

	getCurrPoint(p, startPoint); /*get start point coordinates*/
	memcpy(currentPoint, startPoint, 2 * sizeof(double) ); /*copy start point to current point*/
	
	for (i = 1; i<attractorLimit+attractorIterations; i++) {
		if (isPointInfinite(currentPoint)) {
			fillBasinSlowTrack(p, startPoint, i, IDMC_BASIN_INFINITY);
			break;
		}
		if (!isPointInsideBounds(p, currentPoint)) {
			if (i >= attractorLimit) {
				fillBasinSlowTrack(p, startPoint, i, IDMC_BASIN_INFINITY);
				break;
			}
			else
				continue;
		}

		state = getValue(p, currentPoint);
		
		/* attractor encountered */
		if (isOdd(state) && (state != 0) ) {
			fillBasinSlowTrack(p, startPoint, i - 1, state+1);
			break;
		}
		STEP(currentPoint);
	}
	if(i==(attractorLimit+attractorIterations))
		fillBasinSlowTrack(p, startPoint, i - 1, IDMC_BASIN_INFINITY);
	return IDMC_OK;
}
#undef attractorLimit
#undef attractorIterations
#undef startPoint
#undef currentPoint
#undef state
#undef STEP

int idmc_basin_slow_finished(idmc_basin_slow *p) {
	return basin_finished(p);
}
