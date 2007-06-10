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
#ifndef __RIDMC__
#define __RIDMC__

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

#ifdef RIDMC_DEBUG
#define PDEBUG Rprintf
#else 
#define PDEBUG(...)
#endif

#define RIDMC_ERROR(n) error("[idmclib error: %s] %s\n", idmc_err_message[n], ((idmc_model*) R_ExternalPtrAddr(m))->errorMessage)
#define RIDMC_GENERIC_ERROR(n) error("[idmclib error] %s\n", idmc_err_message[n])

/* Convert C double vector to R real vector, and protect it */
SEXP pdToNumVec(double *p, int len);
/*inverse operation. Vector size is copied in 'out_len' */
double* numVecToPd(SEXP p, int* out_len);
/* Convert C string R char vector of length 1, and protect it */
SEXP pcToStrVec(char *p);

#endif
