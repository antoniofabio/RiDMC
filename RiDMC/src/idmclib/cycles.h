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
#ifndef CYCLES_H
#define CYCLES_H

#ifdef __cplusplus
extern "C" {
#endif

#include "model.h"

int idmc_cycles_find(idmc_model* model, double *parameters, double *start_point, 
	int power, double epsilon, int max_iterations, double* result, double *eigvals);
int idmc_cycles_powf(idmc_model *model, int pow, double* par, double* var, double* ans);
int idmc_cycles_powNumJac(idmc_model *model, int pow, double* par, double* var, double* Jf, double *util);
int idmc_cycles_eigval(double *mat, int dim, double *ans);

#ifdef __cplusplus
}
#endif

#endif
