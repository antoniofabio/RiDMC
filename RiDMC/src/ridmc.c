/*
ridmc: iDMC->R interface

Copyright (C) 2007 Marji Lines and Alfredo Medio.

Written by Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or any
later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Last modified: $Date: 2007-05-19 15:56:19 +0200 (sab, 19 mag 2007) $
*/
#include "ridmc.h"

/* Convert C double vector to R real vector, and protect it */
SEXP pdToNumVec(double *p, int len) {
	SEXP ans;
	PROTECT(ans = allocVector(REALSXP, len));
	memcpy(REAL(ans), p, len * sizeof(double) );
	return ans;
}
/*inverse operation. Vector size is copied in 'out_len' */
double* numVecToPd(SEXP p, int* out_len) {
	*out_len = length(p);
	double *ans = (double*) R_alloc((*out_len), sizeof(double));
	memcpy(ans, REAL(p), (*out_len) * sizeof(double) );
	return ans;
}

/* Convert C string R char vector of length 1, and protect it */
SEXP pcToStrVec(char *p) {
	SEXP ans;
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkChar(p));
	return ans;
}
