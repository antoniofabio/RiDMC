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
#ifndef __BASINS_SLOW_H__
#define __BASINS_SLOW_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "basin_common.h"
#include "defines.h"
#include "model.h"
#include "raster.h"

#define OVERLAP_FACTOR 0.1

typedef struct {
	idmc_model* model; /*model object*/
	double *parameters; /*model parameters*/
	idmc_raster* raster; /*raster data*/
	int attractorLimit; /*no. iterations for transient*/
	int attractorIterations; /*no. iterations for describing an attractor*/
	int ntries; /*no. tries for finding attractors*/
	int nAttractors; /*how many attractors were found*/

	/*Internal data: */
	int dataLength; /*total number of cells*/
	int currId; /*current cell pointer*/
	double *startPoint, *currentPoint, *work; /*work memory spaces*/
	int state; /*support temp variables*/
	int *attractorsSamplePoints; /*buffer with sample ids of attractors already found*/
	int *attractorsCoincidence; /*coincidence information among attractors*/
	int initialized; /*was idmc_basin_slow_init already called?*/
} idmc_basin_slow;

/* Allocate new 'idmc_basin_slow' object
m: model object (object is cloned)
parameters: model parameters (vector will be copied)
xmin, xmax, xres: x axis ranges and resolution
ymin, ymax, yres: y axis ranges and resolution
attractorLimit: limit number of iterations before encountering an attractor
attractorIterations: limit number of iterations for drawing an attractor
out_basin: result object
returns: an integer code as defined in 'defines.h'
*/
int idmc_basin_slow_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres, 
	int attractorLimit, int attractorIterations, int ntries,
	idmc_basin_slow** out_basin);
/*deallocates an idmc_basin object*/
void idmc_basin_slow_free(idmc_basin_slow* p);
/*init basin (find attractors)*/
int idmc_basin_slow_init(idmc_basin_slow* p);
/*do one algorithm step*/
int idmc_basin_slow_step(idmc_basin_slow* p);
/*check if algorithm finished*/
int idmc_basin_slow_finished(idmc_basin_slow* p);

#ifdef __cplusplus
}
#endif

#endif
