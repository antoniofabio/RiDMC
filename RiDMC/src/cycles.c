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
#include <math.h>
#include <string.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_vector_complex.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_eigen.h>
#include "model.h"
#include "cycles.h"

struct root_function_data {
	idmc_model *model;
	double *parameters;
	int power;

	/* cache */
	double *result;
};

/* defines (f^power)(x) - x */
static int root_function(const gsl_vector * x, void *params, gsl_vector * f)
{
	struct root_function_data *data = params;

	for (int i = 0; i < data->model->var_len; i++) {
		data->result[i] = gsl_vector_get(x, i);
	}

	int status;

	/* evaluate (f^n)(x) */
	for (int pow = 0; pow < data->power; pow++) {
		status = idmc_model_f(data->model, data->parameters, data->result, data->result);

        // lua may leave a string on its stack.
        // gsl_multiroot_fsolver_iterate returns GSL_EBADFUNC
        // if this failes
		if (status != IDMC_OK) {
			return GSL_EFAILED;
		}
	}

	for (int i = 0; i < data->model->var_len; i++) {
		gsl_vector_set(f, i, data->result[i] - gsl_vector_get(x, i));
	}

	return GSL_SUCCESS;
}

/* evaluate (f^n)(x) */
int idmc_cycles_powf(idmc_model *model, int pow, double* par, double* var, double* ans)
{
	int i, status;
	double *tmp = (double*) malloc(model->var_len*sizeof(double));
	memcpy(tmp, var, model->var_len*sizeof(double));
	for (i = 0; i < pow; i++) {
		status = idmc_model_f(model, par, tmp, ans);
		memcpy(tmp, ans, model->var_len * sizeof(double));
		if (status != IDMC_OK)
			break;
	}
	free(tmp);
	return status;
}

/*
Computes J(f^n)(x) numerically
Jf should point to an array of size model->var_len * model->var_len
util should point to a workspace memory of size 3*model->var_len
*/
int idmc_cycles_powNumJac(idmc_model *model, int pow, double* par, double* var, double* Jf, double *util)
{
	int i,j;
	int p1 = model->var_len;
	double eps;
	double *util2 = util + p1;
	double *util3 = util2 + p1;
	idmc_cycles_powf(model, pow, par, var, util); /*store F(x0) in 'util'*/
	for(i=0; i<p1; i++) { //for each variable
		memcpy(util2, var, p1 * sizeof(double)); /*store x0*/
		eps = ((var[i] < 1) ? 1: var[i]) * IDMC_EPS_VALUE;
		util2[i] = var[i]+eps;
		idmc_cycles_powf(model, pow, par, util2, util3);
		for(j=0;j<p1; j++) //for each map component, store estimated derivative
			Jf[j*p1+i] = (util3[j] - util[j]) / eps;
	}
	return IDMC_OK;
}

int idmc_cycles_eigval(double *mat, int dim, double *ans) {
  gsl_eigen_nonsymm_workspace *ws;
  gsl_matrix_view A;
  gsl_vector_complex *eval;
  int res;
  eval = gsl_vector_complex_alloc(dim);
  if(eval==NULL)
    return IDMC_EMEM;
  A = gsl_matrix_view_array(mat, dim, dim);
  ws = gsl_eigen_nonsymm_alloc(dim);
  if(ws==NULL)
    return IDMC_EMEM;
  gsl_eigen_nonsymm_params(/*full Shur comp.*/0, /*balance matrix*/1, ws);
  res = gsl_eigen_nonsymm (&A.matrix, eval, ws);
  gsl_vector_view vr = gsl_vector_complex_real (eval);
  gsl_vector_view vi = gsl_vector_complex_imag (eval);
  for(int i=0; i<dim; i++)
    ans[i] = sqrt( gsl_vector_get(&vr.vector, i) * gsl_vector_get(&vi.vector, i) );
  gsl_eigen_nonsymm_free(ws);
  gsl_vector_complex_free(eval);
  return IDMC_OK;
}

int idmc_cycles_find(idmc_model* model, double *parameters, double *start_point, int power, 
	double epsilon, int max_iterations, double* result, double *eigvals) 
{
	const int dim = model->var_len;
	double *Jf, *util;

	struct root_function_data *func_data;

	func_data = calloc(sizeof(struct root_function_data), 1);
	if (func_data == NULL) {
		return IDMC_EMEM;
	}

	func_data->model = model;
	func_data->power = power;
	func_data->parameters = malloc(model->par_len * sizeof(double));
	if (func_data->parameters == NULL) {
		free(func_data);
		return IDMC_EMEM;
	}
	memcpy(func_data->parameters, parameters, model->par_len * sizeof(double));

	func_data->result = malloc(dim * sizeof(double));
	if (func_data->result == NULL) {
		free(func_data->parameters);
		free(func_data);
		return IDMC_EMEM;
	}

	gsl_vector *initial_value;
	initial_value = gsl_vector_alloc(dim);
	if (func_data->result == NULL) {
		free(func_data->result);
		free(func_data->parameters);
		free(func_data);
		return IDMC_EMEM;
	}
	for (int i = 0; i < dim; i++) {
		gsl_vector_set(initial_value, i, start_point[i]);
	}

	gsl_multiroot_function mf;
	mf.f = &root_function;
	mf.n = dim;
	mf.params = func_data;

	const gsl_multiroot_fsolver_type *T = gsl_multiroot_fsolver_hybrids;
	gsl_multiroot_fsolver *solver = gsl_multiroot_fsolver_alloc (T, dim);
	gsl_multiroot_fsolver_set(solver, &mf, initial_value);

	int iter = 0;
	int status;
	do {
		iter++;
		status = gsl_multiroot_fsolver_iterate(solver);
        if (status || model->interrupt) {  /* check if solver is stuck */
			break;
		}
		status = gsl_multiroot_test_residual(solver->f, epsilon);
	} while (status == GSL_CONTINUE && iter < max_iterations);

	for (int i = 0; i < dim; i++) {
		result[i] = gsl_vector_get(solver->x, i);
	}

	gsl_multiroot_fsolver_free(solver);
	gsl_vector_free(initial_value);
	free(func_data->result);
	free(func_data->parameters);
	free(func_data);

	if (model->interrupt) // interrupted
		return IDMC_EINT;
	if (status == GSL_EBADFUNC) // error from model function
		return IDMC_ELUASYNTAX;
	
	Jf = (double*) malloc(model->var_len * model->var_len * sizeof(double));
	util = (double*) malloc(3 * model->var_len * sizeof(double));
	status = idmc_cycles_powNumJac(model, power, parameters, result, Jf, util);
	free(util);
	if(status!=IDMC_OK) {
		free(Jf);
		return status;
	}
	status = idmc_cycles_eigval(Jf, model->var_len, eigvals);
	free(Jf);
	return status;
}
