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
#ifndef matrices_include_guard
#define matrices_include_guard

void vector_out(gsl_vector* v);

void matrix_out(gsl_matrix* m);

void vector_arr_out(double* v,int n);

void matrix_arr_out(double* a,int m,int n);
//only matrices in the heap may be cloned (and then freed)
gsl_matrix* clone_matrix(gsl_matrix* m);

double* clone_array(double* a, int d);

/*matrix(m*n) times vector(n)
  a,b are not affected */
inline void mvproduct(double* a, double* b, double* result,int m,int n);

/*a <- c times a */
inline void mcproduct(double* a,double c,int m,int n);

/*a <- a+b, so a is affected */
inline void sum(double* a, double* b, int d);

/*result <- a times b */
inline void mmproduct(const double* a, const double* b, double* result,int m,int n,int p);

/*v <- c times v */
inline void vcproduct(double* v, double c,int n);

/*b = a' */
inline void transpose(const double* a, double* b, int m,int n);

/*affects a! so we might need to clone a. This is just a wrapper of GSL function
  the result is stored in a and tau.*/
inline void qr_coded(double* a, double* tau, int m,int n);

/*assumes we have a "coded" QR decomposition (a,tau) of the original matrix*/
inline void qr_decomp(double* a, double* tau, double* q, double* r,int m,int n);

/* assumes (a,tau) is the "coded" QR decomp. computes QS, Q=Q(m,m) S=S(m,p)
   affects s (stores there the result) */
inline void qr_qmproduct(double* a, double* tau, 
double* s, int m,int n,int p);

/*same as above but with transpose for q; i.e QtS and not QS */
inline void qr_qtmproduct(double* a, double* tau, 
double* s, int m,int n,int p);

/*forms asymmetric matrix with the same lower triangular part*/
inline void asym(double* a,int n);


#endif /* matrices_include_guard*/

