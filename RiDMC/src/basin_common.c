#include "basin_common.h"

void fillRasterTrack(idmc_raster *r, idmc_model* m,
	double *par, double *startPoint, int iterations, int value, double *workspace) {
	memcpy(workspace, startPoint, (m->var_len) * sizeof(double));
	for(int i=0; i<iterations; i++) {
		if(!idmc_raster_isxyInsideBounds(r, workspace[0], workspace[1]))
			continue;
		idmc_raster_setxy(r, workspace[0], workspace[1], value);
		idmc_model_f(m, par, workspace, workspace);
	}
}
