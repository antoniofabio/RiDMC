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
#ifndef LEXP_H
#define LEXP_H
#ifdef __cplusplus
extern "C" {
#endif

#include "model.h"

int idmc_lexp(idmc_model* model, const double *par, double *startPoint, double *result, int iterations);

int idmc_lexp_ode(idmc_model* model, double *parameters, double *startPoint,
					 double *result, double time, double step);

int idmc_lexp_ode_step(idmc_model* model, double *parameters, double* result,
						  double* Q, double* y, double* t, double step);

#ifdef __cplusplus
}
#endif

#endif
