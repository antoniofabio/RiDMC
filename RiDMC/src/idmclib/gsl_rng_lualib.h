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
#ifndef __gsl_rng_lualib__
#define __gsl_rng_lualib__

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <lua.h>

void initGslRng(lua_State *L);
void freeGslRng(lua_State *L);
/*for retrieving and eventually manipulating RNG directly:*/
gsl_rng* getGslRngState(lua_State *L);

/*to use them in your lua interpreter, you have to call something like:
	initGslRng(L);
	...
	freeGslRng(lua_State *L);
*/

/*internal functions*/
int setGslRngSeed(lua_State *L);
int getGslRngRunif(lua_State *L);
int getGslRngRnorm(lua_State *L);
int getGslRngRpois(lua_State *L);
int getGslRngLaplace(lua_State *L);
int getGslRngExponential(lua_State *L);
int getGslRngBeta(lua_State *L);
int getGslRngLognormal(lua_State *L);
int getGslRngLogistic(lua_State *L);
int getGslRngPareto(lua_State *L);
#endif
