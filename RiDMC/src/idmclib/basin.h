/*
iDMC C library

Copyright (C) 2006,2007 Marji Lines and Alfredo Medio.

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
#ifndef __BASINS_H__
#define __BASINS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "basin_common.h"
#include "defines.h"
#include "model.h"
#include "raster.h"


typedef struct {
	idmc_model* model; /*model object*/
	double *parameters; /*model parameters*/
	idmc_raster* raster; /*raster data*/
	int attractorLimit; /*no. iterations for transient*/
	int attractorIterations; /*no. iterations for describing an attractor*/
	/*Internal data: */
	int dataLength; /*total number of cells*/
	int currId; /*current cell pointer*/
	double *startPoint, *currentPoint, *work; /*work memory spaces*/
	int attractorColor; int basinColor; /*current attractor and basin colors*/
	int index; /*iteration index*/
	int state, attr, color; /*support temp variables*/
} idmc_basin;

/* Allocate new 'idmc_basin' object
m: model object (object is cloned)
parameters: model parameters (vector will be copied)
xmin, xmax, xres: x axis ranges and resolution
ymin, ymax, yres: y axis ranges and resolution
attractorLimit: limit number of iterations before encountering an attractor
attractorIterations: limit number of iterations for drawing an attractor
out_basin: result object
returns: an integer code as defined in 'defines.h'
*/
int idmc_basin_alloc(idmc_model *m, double *parameters,
	double xmin, double xmax, int xres,
	double ymin, double ymax, int yres, 
	int attractorLimit, int attractorIterations,
	idmc_basin** out_basin);
/*deallocates an idmc_basin object*/
void idmc_basin_free(idmc_basin* p);
/*do one algorithm step*/
int idmc_basin_step(idmc_basin* p);
/*check if algorithm finished*/
int idmc_basin_finished(idmc_basin* p);

#ifdef __cplusplus
}
#endif

#endif
