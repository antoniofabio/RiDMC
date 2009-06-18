/*
ridmc: iDMC->R interface

Copyright (C) 2007,2009 Marji Lines and Alfredo Medio.

Written by Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or any
later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

*/
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdlib.h>
#include <gsl/gsl_odeiv.h>
#include <idmclib/model.h>
#include <idmclib/traj.h>

#include "ridmc.h"

SEXP ridmc_bifurcation(SEXP m, SEXP whichVar,
		       SEXP par, SEXP var, SEXP whichPar,
		       SEXP parValues, SEXP transient, SEXP maxPeriod) {
  SEXP ans;
  int np, mp, tr, i, j, ij, varlen;
  double *tv;
  np = length(parValues);
  varlen = length(var);
  mp = INTEGER(maxPeriod)[0];
  tr = INTEGER(transient)[0];
  idmc_model *model;
  model = (idmc_model*) R_ExternalPtrAddr(m);
  PROTECT( ans = allocVector(REALSXP, np * mp ) );
  tv = (double*) R_alloc(length(var), sizeof(double));
  for(i=0, ij=0; i<np; i++) {
    R_CheckUserInterrupt();
    REAL(par)[INTEGER(whichPar)[0]] = REAL(parValues)[i];
    memcpy(tv, REAL(var), length(var)*sizeof(double));
    for(j=0; j<tr; j++)
      idmc_model_f(model, REAL(par), tv, tv);
    for(j=0; j<mp; j++, ij++) {
      idmc_model_f(model, REAL(par), tv, tv);
      REAL(ans)[ij] = tv[INTEGER(whichVar)[0]];
    }
  }
  UNPROTECT(1);
  return ans;
}

/*return true if 'a' and 'b' are closer than 'eps' in maxnorm dist.*/
static int arePointsClose(double* a, double* b, int dim, double eps) {
  double M = fabs(a[0] - b[0]);
  for(int i=1; i<dim; i++)
    M = fmax(M, fabs(a[0] - b[0]));
  return (M <= eps);
}

/*returns true if 'pt' is in set 'pts' with proximity threshold 'eps'*/
static int isPointInSet(double* pts, int npts,
			double* pt, int dim,
			double eps) {
  for(int i=0; i<npts; i++)
    if(arePointsClose(pts + (i*dim), pt, dim, eps))
      return 1;
  return 0;
}

/*adds point 'pt' to set 'pts' if 'pt' doesn't already belongs to it.
Returns new set size.

'pts' is assumed to be already big enough*/
static int addPointToSet(double* pts, int npts, double* pt, int dim, double eps) {
  if(!isPointInSet(pts, npts, pt, dim, eps)) {
    memcpy(pts + npts * dim, pt, dim * sizeof(double));
    return(npts+1);
  }
  return npts;
}

/*
m: idmc model object
par: fixed parameter values
var: common starting value
whichParXY: which parameters to vary
parXValues: vector of x-par values
parYValues: vector of y-par values
transient: transient length
maxPeriod: max period
*/
SEXP ridmc_bifurcation_map(SEXP m,
			   SEXP par, SEXP var,
			   SEXP whichParXY, SEXP parXValues, SEXP parYValues,
			   SEXP transient, SEXP maxPeriod,
			   SEXP in_eps) {
  SEXP ans;
  int npX, npY, mp, tr, i, j, dim, jj;
  int whichX, whichY;
  double *tv, *attractor, eps;
  idmc_model *model;
  npX = length(parXValues);
  npY = length(parYValues);
  whichX = INTEGER(whichParXY)[0];
  whichY = INTEGER(whichParXY)[1];
  dim = length(var);
  mp = INTEGER(maxPeriod)[0];
  tr = INTEGER(transient)[0];
  eps = REAL(in_eps)[0];
  model = (idmc_model*) R_ExternalPtrAddr(m);
  PROTECT( ans = allocVector(INTSXP, npX * npY ) );
  attractor = (double*) R_alloc(dim * mp, sizeof(double));
  tv = (double*) R_alloc(dim, sizeof(double));
  for(i=0; i<npX; i++) {
    REAL(par)[whichX] = REAL(parXValues)[i];
    for(j=0; j<npY; j++) {
      R_CheckUserInterrupt();
      REAL(par)[whichY] = REAL(parYValues)[j];
      memcpy(tv, REAL(var), dim * sizeof(double));
      /*discard transient:*/
      for(jj=0; jj<tr; jj++)
	idmc_model_f(model, REAL(par), tv, tv);
      /*store all subsequent points in the 'attractor' container:*/
      int period = 0;
      for(jj=0; jj<mp; jj++) {
	idmc_model_f(model, REAL(par), tv, tv);
	period = addPointToSet(attractor, period, tv, dim, eps);
      }
      INTEGER(ans)[i * npY + j] = period;
    }
  }
  UNPROTECT(1);
  return ans;
}
