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
#include <string.h>
#include <math.h>
#include <gsl/gsl_block.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_odeiv.h>
#include "lexp_aux.h"
#include "matrices.h"
#include "model.h"

struct combined{
	idmc_model* model;
	int dim;
	double* pars;
	double* data;
	double* draft1;
	double* draft2;
	double* draft3;
};

typedef struct combined Combined;

int func(double t,const double y[],double f[],void* params){
	int status;
	Combined* c=(Combined*) params;
	status=idmc_model_f((*c).model,(*c).pars,y,f);
	if (status==IDMC_OK) 
		return GSL_SUCCESS;
	else
		return GSL_SUCCESS-1;
}	

inline int func_jac(double t,double* y,double* f,Combined* params){
	int status;
	Combined* par=(Combined*) params;
	status=idmc_model_Jf((*par).model,(*par).pars,y,f);
	if (status==IDMC_OK) 
		return GSL_SUCCESS;
	else
		return GSL_SUCCESS-1;
}

//(*c).draft should hold dim*dim
inline int interpolated_jac(Combined* c, double t0, double* y0, double t1, double* y1, double t, double f[])
{
	int status1,status2;
	int dim=(*c).dim;
	double* draft=(*c).draft1;//(double*) malloc(dim*dim*sizeof(double));
	//assumes that y0 y1 hold dim elements each
	
	double a;
	if (t1==t0)
		a=0;
	else
		a=(t-t0)/(t1-t0);
	status1=func_jac(t0,y0,f,c);
	status2=func_jac(t1,y1,draft,c);
	
	gsl_matrix_view m1v = gsl_matrix_view_array(draft,dim,dim);
	gsl_matrix_view m0v = gsl_matrix_view_array(f,dim,dim);
	gsl_matrix_scale(&m1v.matrix,a);
	gsl_matrix_scale(&m0v.matrix,1-a);
	gsl_matrix_add(&m0v.matrix,&m1v.matrix);
	if (status1==GSL_SUCCESS && status2==GSL_SUCCESS) 
		return GSL_SUCCESS;
	else
		return GSL_SUCCESS-1;
}

// (*c).data is 2dim+2, (*c).draft should hold 5*dim*dim (sizeof(double))
int Qsystem (double t, const double Q[], double RHS[], void *params)
{
	int status;
	Combined* c=(Combined*) params;
	int dim=(*c).dim;
	int dd=dim*dim;

	double* ptr=(*c).data;
	double tt0=*ptr;
	double* yy0=ptr+1;
	double tt1=*(ptr+dim+1);
	double* yy1=ptr+(dim+2);


	double* draft=(*c).draft2;//(double*) malloc(4*dim*dim*sizeof(double));
	double* Jac=draft;

	status=interpolated_jac(c,tt0, yy0, tt1, yy1, t, Jac);

	double* Qt=Jac+dd;
	transpose(Q,Qt,dim,dim);

	double* tmp1=Qt+dd;
	mmproduct(Jac,Q,tmp1,dim,dim,dim);
	double* tmp2=tmp1+dd;
	mmproduct(Qt,tmp1,tmp2,dim,dim,dim);
	asym(tmp2,dim);
	mmproduct(Q,tmp2,RHS,dim,dim,dim);

	if (status==GSL_SUCCESS) 
		return GSL_SUCCESS;
	else
		return GSL_SUCCESS-1;
}

inline int elementary_step(Combined* c, double step, gsl_odeiv_step* ode_step, gsl_odeiv_step* Q_step, gsl_odeiv_system* sys,
						   double* t, double* y, double* Q, double* l, double* y_err, double* Q_err ){
	int status1,status2,status3;
	int dim=(*c).dim;
	int dd=dim*dim;

	double* Jac;
	double* Qt;
	double* QtJ;
	double* QtJQ;	
	double* R;
	double* Qcopy;
	double* tmp1;

	*((*c).data)=*t;
	memcpy((*c).data+1,y,dim*sizeof(double));


	status1 = gsl_odeiv_step_apply (ode_step, *t, step, y, y_err, NULL, NULL, sys);

	*((*c).data+dim+1)=*t+step;
	memcpy((*c).data+dim+2,y,dim*sizeof(double));

	gsl_odeiv_system Q_sys={Qsystem, NULL,dim*dim, c};

	status2 = gsl_odeiv_step_apply (Q_step, *t, step,  Q, Q_err, NULL, NULL, &Q_sys);
	*t=*t+step;

	double* draft=(*c).draft3;//(double*) malloc((6*dd+dim)*sizeof(double));
	R=draft;
	tmp1=R+dd;
	Qcopy=tmp1+dim;
	Qt=Qcopy+dd;
	Jac=Qt+dd;
	QtJ=Jac+dd;
	QtJQ=QtJ+dd;

	memcpy(Qcopy,Q,dd*sizeof(double));
	qr_coded( Qcopy, tmp1, dim,dim);
	qr_decomp(Qcopy, tmp1, Q, R,dim,dim);

	status3=idmc_model_Jf((*c).model,(*c).pars,y,Jac);
	transpose(Q,Qt,dim,dim);
	mmproduct(Qt,Jac,QtJ,dim,dim,dim);
	mmproduct(QtJ,Q,QtJQ,dim,dim,dim);
	gsl_vector_view lv=gsl_vector_view_array(l,dim);
	gsl_matrix_view QtJQv=gsl_matrix_view_array(QtJQ,dim,dim);
	gsl_vector_view diagv=gsl_matrix_diagonal(&QtJQv.matrix);
	gsl_vector_scale(&diagv.vector,step);
	gsl_vector_add(&lv.vector,&diagv.vector);

	if (status1==GSL_SUCCESS && status2==GSL_SUCCESS && status3==IDMC_OK) 
		return GSL_SUCCESS;
	else
		return GSL_SUCCESS-1;
}


inline int time_plot_step(idmc_model* model, int dim, double step, double* t, double* pars,
						  double* y, double* Q,  double* l, double* alloc_memory ){
	int s,status;
	status=IDMC_OK;

	int dd=dim*dim;

	const  gsl_odeiv_step_type* step_type= gsl_odeiv_step_rk2;	

	gsl_odeiv_step* ode_step=gsl_odeiv_step_alloc (step_type, dim);
	gsl_odeiv_step* Q_step=gsl_odeiv_step_alloc (step_type, dim*dim);

	gsl_vector_view lv=gsl_vector_view_array(l,dim);
	if (t==0) 
		gsl_vector_set_zero(&lv.vector);

	double* work_space=alloc_memory;
	double* y_err=work_space;

	double* Q_err=y_err+dim;
	double* point_pair=Q_err+dd;

	double* draft1=alloc_memory+3*dim+2+2*dd;
	double* draft2=draft1+dd;
	double* draft3=draft2+4*dd;  	
	gsl_matrix_view Qv=gsl_matrix_view_array(Q,dim,dim);
	if (t==0) 
		gsl_matrix_set_identity(&Qv.matrix);

	Combined c;
	c.model=model;
	c.dim=dim;
	c.pars=pars;
	c.data=point_pair;
	c.draft1=draft1;
	c.draft2=draft2;
	c.draft3=draft3;
	gsl_odeiv_system sys = {func, NULL, dim, &c};

	s=elementary_step(&c, step, ode_step, Q_step, &sys, t, y, Q, l, y_err, Q_err);

	if (s!=GSL_SUCCESS)
		status=IDMC_EMATH;

	gsl_odeiv_step_free(ode_step);
	gsl_odeiv_step_free(Q_step);
	return(status);
}

//y0=y, t0=0, t1 is the final time, l is the array containing the Lyap.exps.
inline int compute_lexp(idmc_model* model, double* par, int dim, 
						double step, double* y, double t1, double* l, double* alloc_memory){
	int s,status;
	status=IDMC_OK;
	
	int dd=dim*dim;
	
	const  gsl_odeiv_step_type* step_type= gsl_odeiv_step_rk2;	
	
	gsl_odeiv_step* ode_step=gsl_odeiv_step_alloc (step_type, dim);
	gsl_odeiv_step* Q_step=gsl_odeiv_step_alloc (step_type, dim*dim);
	
	double t = 0;
	
	gsl_vector_view lv=gsl_vector_view_array(l,dim);
	gsl_vector_set_zero(&lv.vector);
	
	double* work_space=alloc_memory;
	double* y_err=work_space;
	
	double* Q=y_err+dim;
	double* Q_err=Q+dd;
	double* point_pair=Q_err+dd;
	
	double* draft1=alloc_memory+3*dim+2+2*dd;
	double* draft2=draft1+dd;
	double* draft3=draft2+4*dd;

	gsl_matrix_view Qv=gsl_matrix_view_array(Q,dim,dim);
	gsl_matrix_set_identity(&Qv.matrix);

	Combined c;
	c.model=model;
	c.dim=dim;
	c.pars=par;
	c.data=point_pair;
	c.draft1=draft1;
	c.draft2=draft2;
	c.draft3=draft3;
	gsl_odeiv_system sys = {func, NULL, dim, &c};

	while (t < t1){
		s = elementary_step(&c, step, ode_step, Q_step, &sys, &t, 
							 y, Q, l, y_err, Q_err);
		if (s!=GSL_SUCCESS)
			status=IDMC_OK-1;
	}

	gsl_vector_scale(&lv.vector,1/(t1+step));//if t1 is set to zero, does not give an error 
	gsl_odeiv_step_free (ode_step);
	gsl_odeiv_step_free (Q_step);
	return(status);
}

