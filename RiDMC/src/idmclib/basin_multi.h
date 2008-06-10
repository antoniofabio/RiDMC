/*
iDMC C library

Copyright (C) 2007 Marji Lines and Alfredo Medio.

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
#ifndef __BASINS_MULTI_H__
#define __BASINS_MULTI_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "basin_common.h"
#include "defines.h"
#include "model.h"
#include "raster.h"
#include "attractor.h"

typedef struct {
	idmc_model* model; /*model object*/
	double *parameters; /*model parameters*/
	double *startValues; /*starting values ('xvar' and 'yvar' elemtents are ignored)*/
	int xvar, yvar; /*x and y axes variables*/
	idmc_raster* raster; /*raster data*/
	double eps; /*neighborhood treshold used for attractor identification*/
	int attractorLimit; /*no. iterations for transient*/
	int attractorIterations; /*no. iterations for describing an attractor*/
	int ntries; /*no. tries for finding attractors*/
	int nAttractors; /*how many attractors were found*/
	idmc_attractor* attr_head; /*pointer to the head of found attractors list*/

	/*Internal data: */
	int dataLength; /*total number of cells*/
	int currId; /*current cell pointer*/
	double *startPoint, *currentPoint, *work; /*work memory spaces*/
	int state; /*support temp variables*/
	int initialized; /*was idmc_basin_slow_init already called?*/
} idmc_basin_multi;

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
out_basin: result object
returns: an integer code as defined in 'defines.h'
*/
int idmc_basin_multi_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres,
	double eps, int attractorLimit, int attractorIterations, int ntries,
	int xvar, int yvar, double *startValues,
	idmc_basin_multi** out_basin);
/*deallocates an idmc_basin object*/
void idmc_basin_multi_free(idmc_basin_multi* p);
/*find next model attractor*/
int idmc_basin_multi_find_next_attractor(idmc_basin_multi *b);
/*do one algorithm step*/
int idmc_basin_multi_step(idmc_basin_multi* p);
/*check if algorithm finished*/
int idmc_basin_multi_finished(idmc_basin_multi* p);

/*internals*/

#ifdef __cplusplus
}
#endif

#endif
