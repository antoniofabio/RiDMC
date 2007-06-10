/*
iDMC C library

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
#ifndef _IDMC_DEFINES_H
#define _IDMC_DEFINES_H

/* eps value for numerical derivative computation */
#define IDMC_EPS_VALUE 2e-8

/* normal operation */
#define IDMC_OK 0
/* memory allocation */
#define IDMC_EMEM 1
/* syntax error from lua itself */
#define IDMC_ELUASYNTAX 2
/* lua runtime error */
#define IDMC_ERUN 3
/* malformed model */
#define IDMC_EMODEL 4
/* inconsistent state (disaster) */
#define IDMC_EERROR 5
/* algorithm failed */
#define IDMC_EMATH 6
/* interrupted by request */
#define IDMC_EINT 7

#endif
