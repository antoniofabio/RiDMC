#ifndef __BASINS_COMMON_H__
#define __BASINS_COMMON_H__

/*basin of infinity code:*/
#define IDMC_BASIN_INFINITY 1

/*some utility accessor macros*/
#define MODEL(basin) ((basin)->model)
#define RASTER(basin) ((basin)->raster)

/*Internal macros*/
#define getCurrPoint(p, point) \
	do { \
		while ( (p->currId < p->dataLength ) \
			&& ( (RASTER(p)->data[p->currId]) ) ) \
				p->currId++; \
		(point)[0] = idmc_raster_I2x(RASTER(p), p->currId); \
		(point)[1] = idmc_raster_I2y(RASTER(p), p->currId); \
	} while(0)

#define setValue(p, point, value) \
		idmc_raster_setxy(RASTER(p), point[0], point[1], value)

#define getValue(p, point) \
		idmc_raster_getxy(RASTER(p), point[0], point[1])

#define isPointInsideBounds(p, point) \
	idmc_raster_isxyInsideBounds(RASTER(p), point[0], point[1])

#define isPointInfinite(point) \
	((point)[0]==INFINITY || (point)[1]==INFINITY)

#define isOdd(value) ( (value) == ( ((value)/2) * 2 ) )

/*Stopping condition:*/
#define basin_finished(p) ((p)->currId >= ((p)->dataLength))

#endif
