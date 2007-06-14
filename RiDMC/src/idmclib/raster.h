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
#ifndef __RASTER_H__
#define __RASTER_H__
#include "defines.h"

/*
Raster data structures and methods.
This module contains methods for alloc/dealloc a raster object with integer data,
get/set cell values basing on integer or real coordinates.

Naming convention: real coordinates are indicated lowercase (i.e. 'x' and 'y'),
integer coordinates are indicated uppercase (i.e. 'X', 'Y').
So, for example, 'x2X' is a macro for translating a real x coordinate to an integer one.
*/

/*Traslate real coords to integer values*/
#define idmc_raster_x2X(p, x) (int)( ((x) - (p)->xmin)/(p)->xeps )
#define idmc_raster_y2Y(p, y) (int)( ((y) - (p)->ymin)/(p)->yeps )
/*Translate (X,Y)/(x,y) pair to array index and vice-versa*/
#define idmc_raster_XY2I(p, X, Y) ( ((p)->yres - (Y) - 1) * ((p)->xres) + (X) )
#define idmc_raster_xy2I(p, x, y) idmc_raster_XY2I ( p, idmc_raster_x2X(p, x), idmc_raster_y2Y(p, y) )
#define idmc_raster_I2Y(p, I) ( (p)->yres - (I)/(p)->xres - 1 )
#define idmc_raster_I2X(p, I) ( (I) - ((p)->yres - idmc_raster_I2Y(p, I) -1)*(p)->xres )
#define idmc_raster_I2y(p, I) ( ( idmc_raster_I2Y(p, I) + 0.5 ) * (p)->yeps + (p)->ymin )
#define idmc_raster_I2x(p, I) ( ( idmc_raster_I2X(p, I) + 0.5 ) * (p)->xeps + (p)->xmin )

#define idmc_raster_XY2x(p, X, Y) idmc_raster_I2x(p, idmc_raster_XY2I(p, X, Y))
#define idmc_raster_XY2y(p, X, Y) idmc_raster_I2y(p, idmc_raster_XY2I(p, X, Y))

typedef struct {
	void *g_data; /*currently unused*/
	int *data; /*main data block*/
	double xmin; double xrange; double xeps; int xres;/*x axis settings*/
	double ymin; double yrange; double yeps; int yres; /*y axis settings*/
} idmc_raster;

int idmc_raster_alloc(double xmin, double xmax, int xres,
	double ymin, double ymax, int yres, 
	idmc_raster** out_raster);
void idmc_raster_free(idmc_raster* p);

void idmc_raster_setxy(idmc_raster* p, double x, double y, int value);
int idmc_raster_getxy(idmc_raster* p, double x, double y); 
void idmc_raster_setXY(idmc_raster* p, int X, int Y, int value);
int idmc_raster_getXY(idmc_raster* p, int X, int Y);
void idmc_raster_set(idmc_raster* p, int value);

int idmc_raster_isxyInsideBounds(idmc_raster *p, double x, double y);
int idmc_raster_isXYInsideBounds(idmc_raster *p, int x, int y);


#endif
