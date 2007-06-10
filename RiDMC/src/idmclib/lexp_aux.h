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

Last modified: $Date: 2007-04-11 13:54:00 +0200 (mer, 11 apr 2007) $
*/
#include "model.h"

#ifndef __idmc_lexp_aux_include__
#define __idmc_lexp_aux_include__

inline int compute_lexp(idmc_model* model, double* par, int dim, double step, 
double* y, double t1, double* l, double* alloc_memory);

inline int time_plot_step(idmc_model* model, int dim, double step, double* t,
double* pars, double* y, double* Q, double* l, double* alloc_memory);

#endif
