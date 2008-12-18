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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <assert.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include "model.h"
#include "gsl_rng_lualib.h"

char *idmc_err_message[8] = {
	"normal operation",
	"memory allocation",
	"lua syntax error",
	"lua runtime error",
	"malformed model",
	"inconsistent state",
	"algorithm failed",
	"interrupted by request"
};

/*internal*/
static int get_variable(idmc_model *model, const char *name, char **result);
static int idmc_panic (lua_State *L);
static int get_labels(lua_State* state, const char *name, char ***labels);
static inline int create_table(lua_State* L, const int length, char **keys, const  double values[]);
static int eval_function(char *name, idmc_model *model, const double par[], const double var[], double f[]);
static int eval_matrix(char *name, idmc_model *model, const double par[], const double var[], double Jf[]);
static void storeErrorMessage(idmc_model *model);

int idmc_model_alloc(const char* buffer, const int buffer_len, idmc_model **s){
    idmc_model *model;
	lua_State* L;

	/* create model */
	*s = (idmc_model*)calloc(1, sizeof(idmc_model));
	if ((*s) == NULL) {
		return IDMC_EMEM; 
	}
	model = *s;
	model->errorMessage = (char*) malloc(IDMC_MAXSTRLEN * sizeof(char));
	if(model->errorMessage==NULL) {
		free(model);
		model = NULL;
		return IDMC_EMEM;
	}
	model->errorMessage[0] = 0;

	L = luaL_newstate();
	model->L = L;

	jmp_buf *jmpbuf = malloc(sizeof (jmp_buf));
	if (jmpbuf == NULL) { // abort
		free(model);
		lua_close(L);
		model = NULL;
		return IDMC_EMEM;
	}

	if (setjmp(*jmpbuf) != 0) {
		return IDMC_EERROR; // return ffrom longjmp
	}
	lua_pushlightuserdata (L, jmpbuf);
	lua_atpanic(L, &idmc_panic);

	luaL_openlibs(L);

	/*register functions for random numbers generation*/
	initGslRng(L);

	/* load chunk */
	/* lua_buffer has its error printing functions see lauxlib.c */
	int status = luaL_loadbuffer(L, buffer, buffer_len, "");
	if (status == 0) {  /* parse OK? */
		status = lua_pcall(L, 0, LUA_MULTRET, 0);  /* call main */
	}

	if (status != 0) {
		if (status == LUA_ERRSYNTAX) {
			storeErrorMessage(model);
			return IDMC_ELUASYNTAX; // lua syntax error
		}
		else if (status == LUA_ERRMEM) {
			idmc_model_free(model);
			model = NULL;
			return IDMC_EMEM; // memory error
		}
		else {
			return IDMC_EERROR;
		}
	}

	status = get_labels(L, IDMC_IDENT_PARAMETERS, &(model->par));
	if (status < 0) {
		if (status == IDMC_EMEM) {
			idmc_model_free(model);
			model = NULL;
		}
		return status;
	}
	model->par_len = status;

	status = get_labels(L, IDMC_IDENT_VARIABLES, &(model->var));
	if (status < 0) {
		if (status == IDMC_EMEM) {
			idmc_model_free(model);
			model = NULL;
		}
		return status;
	}
	model->var_len = status;

	lua_pushstring(L, IDMC_IDENT_FUNCTION);
	lua_gettable(L, LUA_GLOBALSINDEX);

	if (lua_type(L, -1) != LUA_TFUNCTION) {
		lua_pop(L, 1);
		lua_pushfstring(L, "No function '%s' in model.", IDMC_IDENT_FUNCTION);
		return IDMC_EMODEL;
	}
	lua_pop(L, 1);

	lua_pushstring(L, IDMC_IDENT_FUNCTION_INVERSE);
	lua_gettable(L, LUA_GLOBALSINDEX);
	if (lua_type(L, -1) != LUA_TFUNCTION) {
		model->has_inverse = 0;
	}
	else {
		model->has_inverse = 1;
	}
	lua_pop(L, 1);

	lua_pushstring(L, IDMC_IDENT_JACOBIAN);
	lua_gettable(L, LUA_GLOBALSINDEX);
	if (lua_type(L, -1) != LUA_TFUNCTION) {
		model->has_jacobian = 0;
	}
	else {
		model->has_jacobian = 1;
	}
	lua_pop(L, 1);
	
	model->buffer = (char*) malloc(buffer_len * sizeof(char));
	if(model->buffer) {
		memcpy(model->buffer, buffer, buffer_len);
		model->buflen = buffer_len;
	} else
		model->buflen = 0;
	
	status=1;
	status = status && (get_variable(model, IDMC_IDENT_NAME, &(model->name)) == IDMC_OK);
	status = status && (get_variable(model, IDMC_IDENT_DESC, &(model->desc)) == IDMC_OK);
	status = status && (get_variable(model, IDMC_IDENT_TYPE, &(model->type)) == IDMC_OK);
	
	if(!status) {
		idmc_model_free(model);
		return IDMC_EMODEL;
	}
	
	model->interrupt=0;

	return IDMC_OK;
}

void idmc_model_free(idmc_model *model) {
	if(model!=NULL) {
	if (model->var != NULL) {
		if (model->var[0] != NULL)
			free(model->var[0]);
		free(model->var);
	}

	if (model->par != NULL) {
		if (model->par[0] != NULL)
			free(model->par[0]);
		free(model->par);
	}
	
	if(model->buflen) {
		free(model->buffer);
	}

	if(model->errorMessage != NULL) {
		free(model->errorMessage);
	}

	/* free gsl random number generator*/
	freeGslRng(model->L);

	/* free jmp_buf */
	free(lua_touserdata(model->L, 1));

	/* close lua */
	lua_close(model->L);
	
	free(model->name);
	free(model->desc);
	free(model->type);

	free(model);
	}
}

idmc_model* idmc_model_clone(idmc_model *s) {
	idmc_model *ans;
	int result;
	result = idmc_model_alloc(s->buffer, s->buflen, &ans);
	if(result==IDMC_OK)
		return ans;
	else
		return NULL;
}

static int idmc_panic (lua_State *L) {
	longjmp(*(jmp_buf*)lua_touserdata(L, 1), 1);

	assert(1);
	return 0; // not reached
}

/*
Pops last error message from lua stack and stores it in model errorMessage buffer
*/
static void storeErrorMessage(idmc_model *model) {
	if(lua_strlen(model->L, -1)>12)
		sprintf(model->errorMessage, "%s", lua_tostring(model->L, -1) + 12);
	else
		sprintf(model->errorMessage, "%s", lua_tostring(model->L, -1) + 0);
	lua_pop(model->L, 1);
}

/*
 * From the lua table-array "name" constructs a char array containing
 * the fields of the table. If there is no such table returns DLUA_ETABLE;
 * if a field is not a string returns DLUA_EFIELD. In both cases pushes 
 * an error message on the stack. Otherwise allocates the array and
 * the string memory that you will free before going to bed,
 * returning the array size i.e. the number of labels in it.
 * Note that a nil field in the table gets considered as a "end of table".
 */
static int get_labels(lua_State* L, const char *name, char ***labels)
{
	int n;     // number of labels
	size_t len; // size of strings

	lua_pushstring(L, name);
	lua_gettable(L, LUA_GLOBALSINDEX);

	if (lua_type(L, -1) != LUA_TTABLE) {
		lua_pop(L, 1);
		lua_pushfstring(L, "No '%s' table in model.", name);
		return IDMC_EMODEL;
	}

	/* count and check elements */
	for(n = 1, len = 0; ; n++) {
		lua_rawgeti(L, -1, n);
		if (lua_isstring(L, -1) != 1) {
			break;
		}
		len += lua_strlen(L, -1) + 1;
		lua_pop(L, 1);
	}

	/* last MUST be nil */
	if (lua_isnil(L, -1) != 1) {
		lua_pop(L, 1);
		lua_pushfstring(L, "Non-string  field in table '%s'.", name);
		return IDMC_EMODEL; // error
	}
	lua_pop(L, 1); // pop nil element

	n -=1;

	*labels = malloc(n * sizeof(char*));
	if (*labels == NULL) {
		lua_pop(L, 1); // pop table
		return IDMC_EMEM;
	}

	char *mem = malloc(len);
	if (mem == NULL) {
		lua_pop(L, 1); // pop table
		free(*labels);
		return IDMC_EMEM;
	}

	for(int i = 1; i <= n; i++) {
		lua_rawgeti(L, -1, i);
		strcpy(mem, lua_tostring(L, -1));
		(*labels)[i - 1] = mem;
		mem += lua_strlen(L, -1) + 1;
		lua_pop(L, 1);
	}

	lua_pop(L, 1); // pop table

	return n;
}

static int get_variable(idmc_model *model, const char *name, char **result) {
	lua_State* L = model->L;
	char* tmp;

	lua_pushstring(L, name);
	lua_gettable(L, LUA_GLOBALSINDEX);

	if (lua_type(L, -1) != LUA_TSTRING) {
		lua_pop(L, 1);
		lua_pushfstring(L, "No variable '%s' in model.", name);
		return IDMC_EMODEL;
	}

	tmp = (char*) lua_tostring(L, -1);
	*result = malloc(strlen(tmp) * sizeof(double));
	strcpy(*result, tmp);
	lua_pop(L, 1);

	if (name == NULL) {
		return IDMC_EMEM;
	}

	return 0;
}

/* 
 * evaluate the model function
 */
int idmc_model_f(idmc_model *model,
		   const double par[], const double var[], double f[])
{
	return eval_function(IDMC_IDENT_FUNCTION, model, par, var, f);
}

/* 
 * evaluate the model inverse function
 */
int idmc_model_g(idmc_model *model,
		   const double par[], const double var[], double f[])
{
	return eval_function(IDMC_IDENT_FUNCTION_INVERSE, model, par, var, f);
}

/*
 * calculate the model function  specified by the string "name"
 * with the specified values (var) and parameters (par).
 * return the result in f.
 *
 * On error condition (IDMC_E*) the top of the stack contains the
 * lua error message string.
 * 
 * Note that var and f can point to the same array
 */
static int eval_function(
		char *name, idmc_model *model,
		const double par[], const double var[], double f[])
{
	lua_State* L = model->L;

	jmp_buf *jmpbuf = lua_touserdata(L, 1);
	if (setjmp(*jmpbuf) != 0) {
		return IDMC_EERROR; // return from longjmp
	}
	lua_atpanic(L, &idmc_panic);

	lua_pushstring(L, name);   /* function name */
	lua_gettable(L, LUA_GLOBALSINDEX);

	for (int i = 0; i < model->par_len; i++)
		lua_pushnumber(L, par[i]);
	for (int i = 0; i < model->var_len; i++)
		lua_pushnumber(L, var[i]);

	int err;
	err = lua_pcall(L, 
		model->par_len + model->var_len, // args
		model->var_len,                  // returns
		0);
	if (err != 0) {
		storeErrorMessage(model);
		if (err == LUA_ERRRUN) {
			return IDMC_ERUN;
		}
		else if (err == LUA_ERRMEM) {
			return IDMC_EMEM;
		}
		else if (err == LUA_ERRERR) {
			return IDMC_EERROR; // "impossible" condition
		}
		assert(1);
		return IDMC_EERROR; // not reached
	}
	for (int i = model->var_len - 1; i > -1; i--) {
		if (!lua_isnumber(L, -1)) {
			lua_pop(L, i+1);
			sprintf(model->errorMessage, "non numeric function result");
			return IDMC_ERUN;
		}
		f[i] = lua_tonumber(L, -1);
		lua_pop(L, 1);
	}

	return IDMC_OK;
}

/*
 * evaluate the model jacobian
 */
int idmc_model_Jf(idmc_model *model,
			const double par[], const double var[], double Jf[]) {
	return eval_matrix(IDMC_IDENT_JACOBIAN, model, par, var, Jf);
}

/*
 * evaluate the inverse jacobian
 */
int idmc_model_Jg(idmc_model *model,
			const double par[], const double var[], double Jg[]) {
	return eval_matrix(IDMC_IDENT_INVERSE_JACOBIAN, model, par, var, Jg);
}

/*
 * evaluate the model jacobian numerically
 */
int idmc_model_NumJf(idmc_model *model,
			   const double par[],const double var[], double Jf[], double util[], double util2[], double util3[]) {
	int i,j, tmp;
	int p1 = model->var_len;
	double eps;
	tmp = idmc_model_f(model,  par, var, util); /*store F(x0)*/
	if(tmp!=IDMC_OK)
		return tmp;
	for(i=0; i<p1; i++) { //for each variable
		memcpy(util2, var, p1 * sizeof(double)); /*store x0*/
		eps = ((var[i] < 1) ? 1: var[i]) * IDMC_EPS_VALUE;
		util2[i] = var[i]+eps;
		tmp = idmc_model_f(model,  par, util2, util3);
		if(tmp!=IDMC_OK)
			return tmp;
		for(j=0;j<p1; j++) //for each map component, store estimated derivative
			Jf[j*p1+i] = (util3[j] - util[j]) / eps;
	}
	return IDMC_OK;
}

/*
 * calculate the "name" matrix in the specified point (var) and
 * parameters (par) returning the result in f.
 * On error condition (IDMC_E*) the top of the stack contains the
 * lua error message string.
 */
static int eval_matrix(
		char *name, idmc_model *model,
    const double par[], const double var[], double Jf[])
{
	lua_State* L = model->L;

	jmp_buf *jmpbuf = lua_touserdata(L, 1);
	if (setjmp(*jmpbuf) != 0) {
		return IDMC_EERROR; // return from longjmp
	}
	lua_atpanic(L, &idmc_panic);

	lua_pushstring(L, name);   /* function name */
	lua_gettable(L, LUA_GLOBALSINDEX);

	for (int i = 0; i < model->par_len; i++) {
		lua_pushnumber(L, par[i]);
	}
	for (int i = 0; i < model->var_len; i++) {
		lua_pushnumber(L, var[i]);
	}

	int err;
	err = lua_pcall(L, 
			model->par_len + model->var_len, // args
			model->var_len * model->var_len, // returns
			0);
	if (err != 0) {
		storeErrorMessage(model);
		if (err == LUA_ERRRUN) {
			return IDMC_ERUN;
		}
		else if (err == LUA_ERRMEM) {
			return IDMC_EMEM;
		}
		else if (err == LUA_ERRERR) {
			return IDMC_EERROR; // "impossible" condition
		}
		assert(1);
		return IDMC_EERROR; // not reached
	}

	for (int i = model->var_len * model->var_len - 1; i > -1; i--) {
		if (!lua_isnumber(L, -1)) {
			lua_pop(L, i+1);
			sprintf(model->errorMessage, "non numeric matrix result");
			return IDMC_ERUN;
		}
		Jf[i] = lua_tonumber(L, -1);
		lua_pop(L, 1);
	}

	return IDMC_OK;
}

static inline int create_table(lua_State* L, const int length, char **keys, const double values[]) {
	lua_newtable(L);

	for (int i = 0; i < length; i++) {
		lua_pushstring(L, keys[i]);
		lua_pushnumber(L, values[i]);
		lua_rawset(L, -3);
	}

	return IDMC_OK;
}

int idmc_model_setGslRngSeed(idmc_model *model, int seed) {
	gsl_rng_set(getGslRngState(model->L), seed);
	return IDMC_OK;
}
