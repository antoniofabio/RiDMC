/*
iDMC C library

Adapted from iDMC, Copyright (C) 2006 Marji Lines and Alfredo Medio
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
#include "gsl_rng_lualib.h"

int initGslRng(lua_State *L) {
  gsl_rng* rng_state = gsl_rng_alloc(gsl_rng_taus);
  if(!rng_state)
    return 1;
  /*store in registry:*/
  lua_pushstring(L, "gslRngState");  /* push address */
  lua_pushlightuserdata(L, (void*)rng_state);  /* push value */
  /* registry[&Key] = myNumber */
  lua_settable(L, LUA_REGISTRYINDEX);
  lua_register(L, "setSeed", setGslRngSeed);
  lua_register(L, "runif", getGslRngRunif);
  lua_register(L, "rnorm", getGslRngRnorm);
  lua_register(L, "rpois", getGslRngRpois);
  lua_register(L, "rlaplace", getGslRngLaplace);
  lua_register(L, "rexponential", getGslRngExponential);
  lua_register(L, "rbeta", getGslRngBeta);
  lua_register(L, "rlognormal", getGslRngLognormal);
  lua_register(L, "rlogistic", getGslRngLogistic);
  lua_register(L, "rpareto", getGslRngLogistic);
  return 0;
}

void freeGslRng(lua_State *L) {
	/*TODO: check for null pointer*/
	gsl_rng_free(getGslRngState(L));
}

gsl_rng* getGslRngState(lua_State *L) {
	/* retrieve pointer from lua registry */
	lua_pushstring(L, "gslRngState");  /* key */
	lua_gettable(L, LUA_REGISTRYINDEX);  /* retrieve value */
	return (gsl_rng*) lua_touserdata(L, -1);
}

int setGslRngSeed(lua_State *L) {
	int seed;
	gsl_rng* rng_state = getGslRngState(L);
	seed = (int) lua_tonumber(L, 1);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	gsl_rng_set(rng_state, seed);
	return 0;
}

int getGslRngRunif(lua_State *L) {
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	lua_pushnumber(L, gsl_rng_uniform(rng_state));
	return 1;
}

int getGslRngRpois(lua_State *L) {
	double mu;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	mu = lua_tonumber(L, 1);
	lua_pushnumber(L, gsl_ran_poisson(rng_state, mu));
	return 1;
}

int getGslRngExponential(lua_State *L) {
	double mu;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	mu = lua_tonumber(L, 1);
	lua_pushnumber(L, gsl_ran_exponential(rng_state, mu));
	return 1;
}

int getGslRngLaplace(lua_State *L) {
	double a;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	a = lua_tonumber(L, 1);
	lua_pushnumber(L, gsl_ran_laplace(rng_state, a));
	return 1;
}

int getGslRngBeta(lua_State *L) {
	double a, b;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	a = lua_tonumber(L, 1);	
	b = lua_tonumber(L, 2);
	lua_pushnumber(L, gsl_ran_beta(rng_state, a, b));
	return 1;
}

int getGslRngLognormal(lua_State *L) {
	double zeta, sigma;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	zeta = lua_tonumber(L, 1);
	sigma = lua_tonumber(L, 2);
	lua_pushnumber(L, gsl_ran_lognormal(rng_state, zeta, sigma));
	return 1;
}

int getGslRngLogistic(lua_State *L) {
	double a;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	a = lua_tonumber(L, 1);
	lua_pushnumber(L, gsl_ran_logistic(rng_state, a));
	return 1;
}

int getGslRngPareto(lua_State *L) {
	double a, b;
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	a = lua_tonumber(L, 1);
	b = lua_tonumber(L, 2);
	lua_pushnumber(L, gsl_ran_pareto(rng_state, a, b));
	return 1;
}


int getGslRngRnorm(lua_State *L) {
	gsl_rng* rng_state = getGslRngState(L);
	if(rng_state==0){
		//call error condition
		return 0;
	}
	lua_pushnumber(L, gsl_ran_gaussian(rng_state, 1));
	return 1;
}
