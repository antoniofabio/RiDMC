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
#include <math.h>
#include "model.h"
#include "lexp.h"
#include "lexp_aux.h"

#define NR_END 1   
#define FREE_ARG void*

typedef int word;

typedef struct {
	idmc_model *model;
	const double *parameters;
	double *point;
	double *jacobian;
} idmc_lexp_state;

static int eval_jacobian(idmc_lexp_state *state, double **JacobMatr);

static void MulMatrVec(word Dim, double** Matr, double Vec[], double Rez[])
{
	register int i, j;
	for(i = 0; i < Dim; i++) {
		Rez[i] = 0;
		for(j = 0; j < Dim; j++)
			Rez[i] += Matr[i][j] * Vec[j];
	}
}


static double MulVecVec(word Dim, double Vec1[], double Vec2[])
{
	int register i;
	double Rez;

	Rez = 0;
	for(i = 0; i < Dim; i++) Rez += Vec1[i] * Vec2[i];
	return Rez;

}

static void MulVecNum(word Dim, double Vec[], double Num, double Rez[])
{
	int register i;
	for(i = 0; i < Dim; i++) Rez[i] = Vec[i] * Num;
}


static void SummVec(word Dim, double Vec1[], double Vec2[], double Rez[])
{
	int register i;
	for(i = 0; i < Dim; i++) Rez[i] = Vec1[i] + Vec2[i];
}

static void CopyVec(word Dim, double Vec[], double Rez[])
{
	int register i;
	for(i = 0; i < Dim; i++) Rez[i] = Vec[i];
}


static double NormVec(word Dim, double Vec[])
{
	int register i;
	double Tmp = 0;

	for(i = 0; i < Dim; i++) Tmp += Vec[i] * Vec[i];
	return sqrt(Tmp);

}

static double *dvector(long nl, long nh)
		/* allocate a double vector with subscript range v[nl..nh] */
{
	double *v;

	v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
// if (!v) nrerror("allocation failure in dvector()");
	if (v == NULL) {
		return NULL;
	}
	return v-nl+NR_END;
}


static double **dmatrix(long nrl, long nrh, long ncl, long nch)
		/* allocate a double matrix with subscript range m[nrl..nrh][ncl..nch] */
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	double **m;

	/* allocate pointers to rows */
	m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double*)));
//  if (!m) nrerror("allocation failure 1 in matrix()");
	if (m == NULL) {
		return NULL;
	}
	m += NR_END;
	m -= nrl;

	/* allocate rows and set pointers to them */
	m[nrl]=(double *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
    //if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
	if (m[nrl] == NULL) {
		free(m);
		return NULL;
	}
  
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

	/* return pointer to array of pointers to rows */
	return m;
}

static void free_dvector(double *v, long nl, long nh)
		/* free a double vector allocated with dvector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

static void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch)
		/* free a double matrix allocated by dmatrix() */
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

/*End of supplementar functions */

int idmc_lexp(idmc_model* model, const double *par, double *startPoint, double *result, int iter) {
	idmc_lexp_state state;
	double *jacobian;
	int Dim = model->var_len;
	jacobian = (double*) calloc(Dim*Dim, sizeof(double));
	state.model = model;
	state.parameters = par;
	state.point = startPoint;
	state.jacobian = jacobian;
	
	double **JacobMatr = dmatrix(0,Dim,0,Dim);
	if (JacobMatr == NULL)
		return IDMC_EMEM;

	double **Vectors = dmatrix(0,Dim,0,Dim);
	if (Vectors == NULL) {
		free_dmatrix(JacobMatr,0,Dim,0,Dim);
		return IDMC_EMEM;
	}

	double **Vectors1 = dmatrix(0,Dim,0,Dim);
	if (Vectors1 == NULL) {
		free(jacobian);
		free_dmatrix(Vectors,0,Dim,0,Dim);
		free_dmatrix(JacobMatr,0,Dim,0,Dim);
		return IDMC_EMEM;
	}
    
	double *TmpVec = dvector(0,Dim);
	if (TmpVec == NULL) {
		free(jacobian);
		free_dmatrix(Vectors1,0,Dim,0,Dim);
		free_dmatrix(Vectors,0,Dim,0,Dim);
		free_dmatrix(JacobMatr,0,Dim,0,Dim);
		return IDMC_EMEM;
	}

	double *LyapPrev = dvector(0, Dim);
	if (LyapPrev == NULL) {
		free(jacobian);
		free_dvector(TmpVec,0,Dim);
		free_dmatrix(Vectors1,0,Dim,0,Dim);
		free_dmatrix(Vectors,0,Dim,0,Dim);
		free_dmatrix(JacobMatr,0,Dim,0,Dim);
		return IDMC_EMEM;
	}

	double Tmp, Tmp1;
	word Cont = 1;
	int status;
	int register i, j, k;

	for(i = 0; i < Dim; i++) {
		for(j = 0; j < Dim; j++) {
			Vectors[i][j] = 0;
		}
		Vectors[i][i] = 1;
		LyapPrev[i] = result[i]  = 0;
	}


	for (i = 0; i < iter; i++){
		status = eval_jacobian(&state, JacobMatr);
		if (status != IDMC_OK) {
			break;
		}

		for(j = 0; j < Dim; j++ ) {

			MulMatrVec(Dim, JacobMatr, Vectors[j], Vectors1[j]);

			if(j){
				CopyVec(Dim, Vectors1[j], Vectors[j]);
				for(k = 0; k < j; k++ ) {
					Tmp1 = MulVecVec(Dim, Vectors1[j], Vectors[k]);
					MulVecNum(Dim, Vectors[k], -1 * Tmp1, TmpVec);
					SummVec(Dim, Vectors[j], TmpVec, Vectors[j]);
				}
				Tmp = NormVec(Dim, Vectors[j]);
			}
			else {
				Tmp = NormVec(Dim, Vectors1[j]);
				CopyVec(Dim, Vectors1[j], Vectors[j]);
			}

			MulVecNum(Dim, Vectors[j], 1. / Tmp, Vectors[j]);
			result[j] += log(Tmp);

		}

	}

	for(i = 0; i < Dim; i++)
		result[i] /= iter;

	free(jacobian);
	free_dmatrix(JacobMatr,0,Dim,0,Dim);
	free_dmatrix(Vectors,0,Dim,0,Dim);
	free_dmatrix(Vectors1,0,Dim,0,Dim);
	free_dvector(TmpVec,0,Dim);
	free_dvector(LyapPrev,0,Dim);

	if (status != IDMC_OK) {
		return status;
	}
	else if (!Cont) {
		return IDMC_EMATH;
	}

	return IDMC_OK;
}

static int eval_jacobian(idmc_lexp_state *state, double **JacobMatr)
{
	int status;
	double *util, *util2, *util3;

	if(state->model->has_jacobian) {
		status = idmc_model_Jf(state->model, 
						 state->parameters,
						 state->point,
						 state->jacobian);
	} else {
		util = (double*) malloc((size_t) (state->model->var_len * sizeof(double)) );
		if(!util)
			return IDMC_EMEM;
		util2 = (double*) malloc((size_t) (state->model->var_len * sizeof(double)) );
		if(!util2) {
			free(util);
			return IDMC_EMEM;
		}
		util3 = (double*) malloc((size_t) (state->model->var_len * sizeof(double)) );
		if(!util2) {
			free(util);
			free(util2);
			return IDMC_EMEM;
		}
		status = idmc_model_NumJf(state->model, state->parameters, state->point, state->jacobian, util, util2, util3);
		free(util);
		free(util2);
		free(util3);
	}

	if (status != IDMC_OK)
		return status;

	const int dim = state->model->var_len; 

	for(int i = 0; i < dim; i++)
		for(int j = 0; j < dim; j++)
			JacobMatr[i][j] = *(state->jacobian + (j + dim * i));

	/* calculate next point */
	status = idmc_model_f(state->model, state->parameters, state->point, state->point);

	return status;
}

int idmc_lexp_ode(idmc_model* model, double *parameters, double *startPoint,
					 double *result, double time, double step)
{
	const int dim = model->var_len;
	double* alloc_memory=(double*) malloc((4*dim+2+13*dim*dim)*sizeof(double));
	if (alloc_memory==NULL) 
		return IDMC_EMEM;	

	int status=compute_lexp(model, parameters, dim, step,
						startPoint, time, result, alloc_memory);
	
	free(alloc_memory);
	return status;//if successful returns IDMC_OK
}


int idmc_lexp_ode_step(idmc_model* model, double *parameters, double* result,
						  double* Q, double* y, double* t, double step)
{
	int status;
	const int dim = model->var_len;
	
	double* alloc_memory=(double*) malloc((4*dim+2+13*dim*dim)*sizeof(double));
	if (alloc_memory==NULL) 
		return IDMC_EMEM;	
	
	status=time_plot_step(model, dim, step, t,
			parameters, y, Q, result, alloc_memory);
	
	free(alloc_memory);
	
	return status;//if successful returns IDMC_OK
}

