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
#include <gsl/gsl_block.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include "matrices.h"

void vector_out(gsl_vector* v){
	int i;
	int d=(*v).size;
	printf("(");
	for (i=0;i<d;i++){
		printf("%.5e ",gsl_vector_get(v,i));
	}	
	printf(")\n");
}

void matrix_out(gsl_matrix* m){
	printf("[");
	int i,j;
	int d1=(*m).size1;
	int d2=(*m).size2;
	for (i=0;i<d1;i++){
		for (j=0;j<d2;j++){
			printf("%.5e ",gsl_matrix_get(m,i,j));
		}
		if (i<d1-1) 
			printf("\n");
		else
			printf("]\n");
	}	
}

void vector_arr_out(double* v,int n){
	gsl_vector_view vv=gsl_vector_view_array(v,n);
	vector_out(&vv.vector);
}

void matrix_arr_out(double* v,int m,int n){
	gsl_matrix_view mv=gsl_matrix_view_array(v,m,n);
	matrix_out(&mv.matrix);
}

/*only matrices in the heap may be cloned (and then freed) */
gsl_matrix* clone_matrix(gsl_matrix* m){
	gsl_matrix* m1=gsl_matrix_alloc((*m).size1,(*m).size2);
	gsl_matrix_memcpy(m1,m);
	return m1;
}

double* clone_array(double* a, int d){
	int dd=d*sizeof(double);
	double* b=(double*) malloc(dd);
	memcpy(b,a,dd);
	return b;	
}

/*matrix(m*n) times vector(n)
  a,b are not affected */
inline void mvproduct(double* a, double* b, double* result,int m,int n){
	gsl_vector_view vv=gsl_vector_view_array(b,n);
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	gsl_vector_view rv=gsl_vector_view_array(result,m);
	gsl_blas_dgemv(CblasNoTrans,1.0,&(av.matrix),&(vv.vector),0.0,&(rv.vector)); 
}

/*a <- a+b, so a is affected */
inline void sum(double* a, double* b, int d){
	gsl_vector_view av=gsl_vector_view_array(a,d);
	gsl_vector_view bv=gsl_vector_view_array(b,d);
	gsl_vector_add(&av.vector,&bv.vector);
}

/*result <- a times b */
inline void mmproduct(const double* a, const double* b, double* result,int m,int n,int p){
	gsl_matrix_view bv=gsl_matrix_view_array((double*) b,n,p);
	gsl_matrix_view av=gsl_matrix_view_array((double*) a,m,n);
	gsl_matrix_view rv=gsl_matrix_view_array(result,m,p);
	gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,&(av.matrix),&(bv.matrix),0.0,&(rv.matrix)); 
}

/*a <- c times a */
inline void mcproduct(double* a,double c,int m,int n){
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	gsl_matrix_scale(&av.matrix,c);
}

/*v <- c times v */
inline void vcproduct(double* v, double c,int n){
	gsl_vector_view vv=gsl_vector_view_array(v,n);
	gsl_vector_scale(&vv.vector,c);
}

/*b = a' */
inline void transpose(const double* a, double* b, int m,int n){
	gsl_matrix_view av=gsl_matrix_view_array((double*) a,m,n);
	gsl_matrix_view bv=gsl_matrix_view_array(b,n,m);
	gsl_matrix_transpose_memcpy(&bv.matrix,&av.matrix);
}

/*forms asymmetric matrix with the same lower triangular part*/
inline void asym(double* a,int n){
	int i;
	gsl_matrix_view av=gsl_matrix_view_array(a,n,n);
	for (i=1;i<n;i++){
		gsl_vector_view vsup=gsl_matrix_superdiagonal(&av.matrix,i);
		gsl_vector_view vsub=gsl_matrix_subdiagonal(&av.matrix,i);
		gsl_vector_memcpy(&vsup.vector,&vsub.vector);
		gsl_vector_scale(&vsup.vector,-1);
	}
	gsl_vector_view vsup=gsl_matrix_superdiagonal(&av.matrix,0);
	gsl_vector_set_zero(&vsup.vector);
}


/*affects a! so we might need to clone a. This is just a wrapper of GSL function
  the result is stored in a and tau.*/
inline void qr_coded(double* a, double* tau, int m,int n){
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	int d;
	if (m<n) d=m; else d=n;
	gsl_vector_view tv=gsl_vector_view_array(tau,d);	
	gsl_linalg_QR_decomp(&av.matrix,&tv.vector);
} 

/*assumes we have a "coded" QR decomposition (a,tau) of the original matrix*/
inline void qr_decomp(double* a, double* tau, double* q, double* r,int m,int n){
	gsl_matrix_view qv=gsl_matrix_view_array(q,m,m);
	gsl_matrix_view rv=gsl_matrix_view_array(r,m,n);
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	int d;
	if (m<n) d=m; else d=n;
	gsl_vector_view tv=gsl_vector_view_array(tau,d);	
	gsl_linalg_QR_unpack(&av.matrix,&tv.vector,&qv.matrix,&rv.matrix);
} 


/* assumes (a,tau) is the "coded" QR decomp. computes QS, Q=Q(m,m) S=S(m,p)
   affects s (stores there the result) */
inline void qr_qmproduct(double* a, double* tau, 
double* s, int m,int n,int p){
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	gsl_matrix_view sv=gsl_matrix_view_array(s,m,p);
	int d;
	if (m<n) d=m; else d=n;
	gsl_vector_view tv=gsl_vector_view_array(tau,d);	
	int i;
	for (i=0;i<p;i++){
		gsl_vector_view scv=gsl_matrix_column(&sv.matrix,i);
		gsl_linalg_QR_Qvec(&av.matrix,&tv.vector,&scv.vector);
	}
}

/*same as above but with transpose for q; i.e QtS and not QS */
inline void qr_qtmproduct(double* a, double* tau, 
double* s, int m,int n,int p){
	gsl_matrix_view av=gsl_matrix_view_array(a,m,n);
	gsl_matrix_view sv=gsl_matrix_view_array(s,m,p);
	int d;
	if (m<n) d=m; else d=n;
	gsl_vector_view tv=gsl_vector_view_array(tau,d);	
	int i;
	for (i=0;i<p;i++){
		gsl_vector_view scv=gsl_matrix_column(&sv.matrix,i);
		gsl_linalg_QR_QTvec(&av.matrix,&tv.vector,&scv.vector);
	}
}
