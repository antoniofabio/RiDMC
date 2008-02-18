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
#ifndef TRAJ_H
#define TRAJ_H
#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>
#include <gsl/gsl_odeiv.h>
#include "model.h"

#define ROWCOL(i, j, numcols) (i)*(numcols) + (j)

/*discrete trajectory*/
typedef struct {
	idmc_model *model;
	double *par;  /*parameters vector*/
	double *var; /*current value*/
	int step; /*current step number*/
} idmc_traj_trajectory;

int idmc_traj_trajectory_alloc(idmc_model *model, double *parValues, double *varValues, idmc_traj_trajectory **ans);
void idmc_traj_trajectory_free(idmc_traj_trajectory *traj);
int idmc_traj_trajectory_step(idmc_traj_trajectory *traj);

/*continuous trajectory*/
typedef struct {
	idmc_model *model;
	double *par;  /*parameters vector*/
	double *var; /*current value*/
	double *error;
	double step_size;
	gsl_odeiv_step_type *step_function_code;
	
	gsl_odeiv_step *step_function; /*used internally*/
	gsl_odeiv_system system; /*used internally*/
} idmc_traj_ctrajectory;

int idmc_traj_ctrajectory_alloc(idmc_model *model, double *parValues, double *varValues, 
						  double step_size, gsl_odeiv_step_type *step_function_code,
						  idmc_traj_ctrajectory **ans);
void idmc_traj_ctrajectory_free(idmc_traj_ctrajectory *trajectory);
int idmc_traj_ctrajectory_step(idmc_traj_ctrajectory *trajectory);

#ifdef __cplusplus
}
#endif

#endif
