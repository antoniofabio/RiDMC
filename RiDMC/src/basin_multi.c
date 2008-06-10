/*
iDMC C library

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

MULTIVARIATE (dim > 2) BASINS ALGORITM
*/
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include "defines.h"
#include "gsl_rng_lualib.h"
#include "raster.h"
#include "model.h"
#include "basin_common.h"
#include "attractor.h"
#include "basin_multi.h"

#define PAR_LEN(m) (m)->par_len
#define VAR_LEN(m) (m)->var_len

int idmc_basin_multi_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres,
	double eps, int attractorLimit, int attractorIterations, int ntries,
	int xvar, int yvar, double *startValues,
	idmc_basin_multi** out_basin) {
	int i;
	idmc_basin_multi* ans;
	idmc_raster* raster;

	if(VAR_LEN(m) < 2)
		return IDMC_EMATH;

	ans = (idmc_basin_multi*) malloc( sizeof(idmc_basin_multi) );
	if(ans==NULL)
		return IDMC_EMEM;

	ans->model = idmc_model_clone(m);
	if(ans->model==NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}

	ans->parameters = (double*) malloc( PAR_LEN(m) * sizeof(double));
	if(ans->parameters==NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->parameters, parameters, PAR_LEN(m) * sizeof(double));

	i = idmc_raster_alloc(xmin, xmax, xres, ymin, ymax, yres, &raster);
	if(i != IDMC_OK) {
		idmc_basin_multi_free(ans);
		return i;
	}
	idmc_raster_set(raster, 0);
	ans->dataLength = xres * yres;
	ans->currId = 0;
	ans->raster = raster;

	ans->eps = fabs(eps);
	ans->attractorLimit = attractorLimit;
	ans->attractorIterations = attractorIterations;
	ans->ntries = ntries;
	ans->xvar = xvar;
	ans->yvar = yvar;

	ans->startValues = (double*) malloc( VAR_LEN(m) * sizeof(double));
	if(ans->startValues == NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->startValues, startValues, VAR_LEN(m) * sizeof(double));

	ans->currentPoint = (double*) malloc(VAR_LEN(m) * sizeof(double));
	if(ans->currentPoint==NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}
	ans->startPoint = (double*) malloc(VAR_LEN(m) * sizeof(double));
	if(ans->startPoint==NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}
	ans->work = (double*) malloc(VAR_LEN(m)*sizeof(double));
	if(ans->work==NULL) {
		idmc_basin_multi_free(ans);
		return IDMC_EMEM;
	}

	ans->attr_head = NULL;

	*out_basin = ans;
	return IDMC_OK;
}

void idmc_basin_multi_free(idmc_basin_multi* p) {
	if(p->model!=NULL)
		idmc_model_free(MODEL(p));
	if(p->parameters!=NULL)
		free(p->parameters);
	if(p->raster!=NULL)
		idmc_raster_free(RASTER(p));
	if(p->startValues!=NULL)
		free(p->startValues);
	if(p->currentPoint!=NULL)
		free(p->currentPoint);
	if(p->work!=NULL)
		free(p->work);
	if(p->startPoint!=NULL)
		free(p->startPoint);
	if(p->attr_head!=NULL)
		idmc_attractor_list_free(p->attr_head);
	free(p);
}

static int is_finite(double *pt, int dim) {
	for(int i=0; i < dim; i++)
		if(isnan(pt[i]) || (fabs(pt[i]) == 1.0/0.0))
			return 0;
	return 1;
}

#define STEP(pt) idmc_model_f(m, b->parameters, (pt), (pt))

#define exit_ok \
	if(1) {\
	free(pt);\
	return IDMC_OK;\
	}

/*Find next attractor, add it to attractors list*/
int idmc_basin_multi_find_next_attractor(idmc_basin_multi *b) {
	idmc_model *m = MODEL(b);
	idmc_raster *r = RASTER(b);
	gsl_rng* rng = getGslRngState(m->L);
	int i, a_id;
	idmc_attractor* al = b->attr_head;
	idmc_attractor* ac;
	double *pt = (double*) malloc(sizeof(double) * VAR_LEN(m));
	memcpy(pt, b->startPoint, sizeof(double) * VAR_LEN(m));
	double eps = b->eps;
	int xvar = b->xvar;
	int yvar = b->yvar;
	int attractorIndex;
	/*start assuming this is a new attractor*/
	if(!al)
		attractorIndex = 0;
	else {
		attractorIndex = idmc_attractor_list_length(al);
		ac = al;
	}

	/*set a random starting point inside the user defined grid*/
	pt[xvar] = r->xmin + gsl_rng_uniform(rng) * (r->xrange);
	pt[yvar] = r->ymin + gsl_rng_uniform(rng) * (r->yrange);

	/*do transient iterations*/
	for(i = 0; i < b->attractorLimit; i++) {
		STEP(pt);
		if(!is_finite(pt, VAR_LEN(m)))
			exit_ok;
	}

	/*run iterations in the attracting region*/
	for(i = 0; i < b->attractorIterations; i++) {
		STEP(pt);
		if(!is_finite(pt, VAR_LEN(m)))
			exit_ok;
		/*still no attractors: create new, with current point as the only point*/
		if(!al) {
			al = ac = idmc_attractor_new(VAR_LEN(m));
			idmc_attractor_hd_set(ac, idmc_attractor_point_new(pt, VAR_LEN(m)));
			continue;
		}
		/*check to what attractor belongs the current point*/
		a_id = idmc_attractor_list_check_point(al, pt, eps);
		/*the point doesn't belong to any known attractor:*/
		/* add the point to the currently set attractor*/
		if(a_id >= idmc_attractor_list_length(al)) {
			if(attractorIndex >= idmc_attractor_list_length(al)) {
				ac = idmc_attractor_new(VAR_LEN(m));
				idmc_attractor_hd_set(ac, idmc_attractor_point_new(pt, VAR_LEN(m)));
				idmc_attractor_list_append(al, ac);
				continue;
			}
			idmc_attractor_point_add(idmc_attractor_point_last(ac->hd),
				idmc_attractor_point_new(pt, VAR_LEN(m)));
			continue;
		}
		/*the point belongs to another, previously found, attractor*/
		if(a_id != attractorIndex) {
			if(attractorIndex >= idmc_attractor_list_length(al))
				continue;
			/*merge the two attractors into one (that discovered first)*/
			idmc_attractor_list_merge(al, a_id, attractorIndex);
			/*update current attractor index and pointer*/
			attractorIndex = (a_id < attractorIndex) ? a_id : attractorIndex;
			ac = idmc_attractor_list_get(al, attractorIndex);
		}
	}

	b->attr_head = al;
	exit_ok;
}

static void fillBasinMultiTrack(idmc_basin_multi *p, double *startPoint,
	int iterations, int value) {
	double *work = p->work;
	idmc_model *m = MODEL(p);
	idmc_raster *r = RASTER(p);
	memcpy(work, startPoint, VAR_LEN(m) * sizeof(double));
	for(int i=0; i<iterations; i++) {
		if(!idmc_raster_isxyInsideBounds(r, work[p->xvar], work[p->yvar]))
			continue;
		idmc_raster_setxy(r, work[p->xvar], work[p->yvar], value);
		idmc_model_f(m, p->parameters, work, work);
	}
}

/*was defined in basin_common*/
#undef getCurrPoint
static void getCurrPoint(idmc_basin_multi *p, double *point) {
	while ( (p->currId < p->dataLength )
		&& ( (RASTER(p)->data[p->currId]) ) )
			p->currId++;
	point[p->xvar] = idmc_raster_I2x(RASTER(p), p->currId);
	point[p->yvar] = idmc_raster_I2y(RASTER(p), p->currId);
}

/* Iterates one cell in the basin grid */
/*some utility definitions:*/
#define attractorLimit (p->attractorLimit)
#define attractorIterations (p->attractorIterations)
#define startPoint (p->startPoint)
#define currentPoint (p->currentPoint)
#define state (p->state)
#define checkPoint(x) idmc_attractor_list_check_point(p->attr_head, (x), p->eps)
#define isPointFinite(x) is_finite((x), nvar)
int idmc_basin_multi_step(idmc_basin_multi* p) {
	if( idmc_basin_multi_finished(p) ) /*algorithm ended*/
		return IDMC_OK;
	idmc_model *m = MODEL(p);
	idmc_basin_multi* b=p; /*just an alias*/
	int i;
	int nvar = m->var_len;
	int ntot = idmc_attractor_list_length(p->attr_head);

	getCurrPoint(p, startPoint); /*get start point coordinates*/
	memcpy(currentPoint, startPoint, nvar * sizeof(double) ); /*copy start point to current point*/
	
	for (i = 1; i<attractorLimit+attractorIterations; i++) {
		if (!isPointFinite(currentPoint)) {
			fillBasinMultiTrack(p, startPoint, i, IDMC_BASIN_INFINITY);
			break;
		}
		state = checkPoint(currentPoint);

		/* attractor encountered */
		if (state < ntot) {
			fillBasinMultiTrack(p, startPoint, i, (state*2)+2);
			break;
		}
		STEP(currentPoint);
	}
	if(i==(attractorLimit+attractorIterations))
		fillBasinMultiTrack(p, startPoint, i - 1, IDMC_BASIN_INFINITY);
	return IDMC_OK;
}
#undef isPointFinite
#undef checkPoint
#undef attractorLimit
#undef attractorIterations
#undef startPoint
#undef currentPoint
#undef state
#undef STEP

int idmc_basin_multi_finished(idmc_basin_multi *p) {
	return basin_finished(p);
}

#undef PAR_LEN
#undef VAR_LEN
