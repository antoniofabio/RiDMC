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
#include <stdlib.h>
#include <string.h>
#include "raster.h"

int idmc_raster_alloc(double xmin, double xmax, int xres,
	double ymin, double ymax, int yres,
	idmc_raster** out_raster) 
{
	idmc_raster *ans = (idmc_raster*) malloc( sizeof(idmc_raster) );
	if(ans==NULL)
		return IDMC_EMEM;
	ans->data = (int*) malloc(xres * yres * sizeof(int));
	if(ans->data==NULL) {
		idmc_raster_free(ans);
		return IDMC_EMEM;
	}
	ans->xmin = xmin;
	ans->xrange = xmax - xmin;
	ans->xeps = ans->xrange / (double) xres;
	ans->xres = xres;
	ans->ymin = ymin;
	ans->yrange = ymax - ymin;
	ans->yeps = ans->yrange / (double) yres;
	ans->yres = yres;
	*out_raster = ans;
	return IDMC_OK;
}

void idmc_raster_free(idmc_raster* p)
{
	if(p->data!=NULL)
		free(p->data);
	free(p);
}

void idmc_raster_setxy(idmc_raster* p, double x, double y, int value)
{
	p->data[ idmc_raster_xy2I ( p, x, y ) ] = value;
}
int idmc_raster_getxy(idmc_raster* p, double x, double y) 
{
	return p->data[ idmc_raster_xy2I ( p, x, y ) ];
}

void idmc_raster_setXY(idmc_raster* p, int x, int y, int value)
{
	p->data[ idmc_raster_XY2I ( p, x, y ) ] = value;
}
int idmc_raster_getXY(idmc_raster* p, int x, int y) 
{
	return p->data[ idmc_raster_XY2I ( p, x, y ) ];
}

void idmc_raster_set(idmc_raster* p, int value) 
{
	memset(p->data, value, (p->xres * p->yres) * sizeof(int));
}

int idmc_raster_isXYInsideBounds(idmc_raster *p, int x, int y) 
{
	return (x>=0 && y>=0 && x<(p->xres) && y<(p->yres));
}

int idmc_raster_isxyInsideBounds(idmc_raster *p, double x, double y) 
{
	return idmc_raster_isXYInsideBounds( p, idmc_raster_x2X(p, x),  idmc_raster_y2Y(p, y) );
}

