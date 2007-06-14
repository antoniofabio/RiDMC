/*
iDMC C library

Adapted from iDMC, Copyright (C) 2004-2006 Marji Lines and Alfredo Medio
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
#include <stdlib.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_odeiv.h>

#include "model.h"
#include "traj.h"

#define SAFE_FREE(p) if(p!=NULL) free(p)

int idmc_traj_trajectory_alloc(idmc_model *model, double *parValues, double *varValues, idmc_traj_trajectory **out_ans) {
	idmc_traj_trajectory *ans;
	*out_ans = (idmc_traj_trajectory *) calloc(1, sizeof(idmc_traj_trajectory));
	if((*out_ans)==NULL)
		return IDMC_EMEM;
	ans = *out_ans;

	ans->model = idmc_model_clone(model);
	if(!(ans->model)) {
		idmc_traj_trajectory_free(ans);
		return IDMC_EMEM;
	}
	ans->par = (double*) malloc(ans->model->par_len * sizeof(double));
	if(!(ans->par)) {
		idmc_traj_trajectory_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->par, parValues, model->par_len * sizeof(double));
	ans->var = (double*) malloc(ans->model->var_len * sizeof(double));
	if(!ans->var) {
		idmc_traj_trajectory_free(ans);
		return IDMC_EMEM;
	}
	memcpy(ans->var, varValues, model->var_len * sizeof(double));
	ans->step = 0;
	*out_ans = ans;
	return IDMC_OK;
}

void idmc_traj_trajectory_free(idmc_traj_trajectory *traj){
	if(traj->model != NULL)
		idmc_model_free(traj->model);
	SAFE_FREE(traj->par);
	SAFE_FREE(traj->var);
	SAFE_FREE(traj);
}

int idmc_traj_trajectory_step(idmc_traj_trajectory *t) {
	int result = idmc_model_f(t->model, t->par, t->var /*from point*/, t->var /*to point*/);
	(t->step)++;
	return result;
}

/*Continuous systems*/
static int odeiv_function(double t, const double yy[], double dydt[], void * params);

int idmc_traj_ctrajectory_alloc(idmc_model *model, double *parValues, double *varValues, 
	double step_size, gsl_odeiv_step_type *step_function_code, idmc_traj_ctrajectory **ans) {
	idmc_traj_ctrajectory *trajectory;
	
	if(model->type[0] != 'C')
		return IDMC_EMATH;

	trajectory = (idmc_traj_ctrajectory *) malloc(sizeof (idmc_traj_ctrajectory));
	if (trajectory == NULL) {
		return IDMC_EMEM;
	}
	/* model */
	trajectory->model = idmc_model_clone(model);
	if(!trajectory->model) {
		idmc_traj_ctrajectory_free(trajectory);
		return IDMC_EMEM;
	}

	/* parameters */
	trajectory->par = malloc(model->par_len * sizeof(double));
	if (trajectory->par == NULL) {
		idmc_traj_ctrajectory_free(trajectory);
		return IDMC_EMEM;
	}
	memcpy(trajectory->par, parValues, model->par_len * sizeof(double));

	/* initial values */
	trajectory->var = (double*) malloc(model->var_len * sizeof(double));
	if (trajectory->var == NULL) {
		idmc_traj_ctrajectory_free(trajectory);
		return IDMC_EMEM;
	}
	memcpy(trajectory->var, varValues, model->var_len * sizeof(double));

	/* initial values */
	trajectory->error = (double*) malloc(model->var_len * sizeof(double));
	if (trajectory->error == NULL) {
		idmc_traj_ctrajectory_free(trajectory);
		return IDMC_EMEM;
	}
	
	trajectory->step_function_code = step_function_code;
	trajectory->step_function = gsl_odeiv_step_alloc(step_function_code, model->var_len);
	if (trajectory->step_function == NULL) {
		idmc_traj_ctrajectory_free(trajectory);
		return IDMC_EMEM;
	}
	
	trajectory->step_size = step_size;
	
	/* gsl ode system */
	trajectory->system.function = odeiv_function;
	trajectory->system.jacobian = NULL; // FIXME
	trajectory->system.dimension = model->var_len;
	trajectory->system.params = trajectory;
	
	*ans = trajectory;

	return IDMC_OK;
}

void idmc_traj_ctrajectory_free(idmc_traj_ctrajectory *trajectory) {
	if(trajectory->model!=NULL)
		idmc_model_free(trajectory->model);
	if (trajectory->step_function!=NULL)
		gsl_odeiv_step_free(trajectory->step_function);
	SAFE_FREE(trajectory->error);
	SAFE_FREE(trajectory->var);
	SAFE_FREE(trajectory->par);
	SAFE_FREE(trajectory);
}

int idmc_traj_ctrajectory_step(idmc_traj_ctrajectory *trajectory) {
	return gsl_odeiv_step_apply(trajectory->step_function, 0, trajectory->step_size,
		trajectory->var, trajectory->error, NULL, NULL, &trajectory->system);
}

static int odeiv_function(double t, const double yy[], double dydt[], void * params) {
	idmc_traj_ctrajectory *trajectory = params;
	int err;

	err = idmc_model_f(trajectory->model, trajectory->par, yy, dydt);
	if (err != IDMC_OK)
		return GSL_EFAILED;
	return GSL_SUCCESS;
}
