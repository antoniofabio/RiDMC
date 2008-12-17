/*
iDMC C library

Copyright (C) 2008 Marji Lines and Alfredo Medio.

Written by Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or any
later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

ATTRACTORS HANDLING FUNCTIONS
*/
#ifndef __ATTRACTOR_H__
#define __ATTRACTOR_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <memory.h>
#include <stdlib.h>

/*max number of attractor points to check for while checking if a points
lies on an attractor*/
#define MAX_ATTR_CHECK_LENGTH 100

struct attractor_pt {
	double *x;
	struct attractor_pt* next;
};
typedef struct attractor_pt idmc_attractor_point;

struct attr_lst {
	idmc_attractor_point *hd; /*head of points list*/
	int dim; /*points dimension*/
	struct attr_lst* next;
	struct attr_lst* previous;
};
typedef struct attr_lst idmc_attractor;

idmc_attractor_point *idmc_attractor_point_new(double* x, int n);
idmc_attractor_point* idmc_attractor_point_clone(idmc_attractor_point* head, int dim);
void idmc_attractor_point_free(idmc_attractor_point* p);
idmc_attractor_point* idmc_attractor_point_add(idmc_attractor_point* last, idmc_attractor_point* p);
idmc_attractor_point* idmc_attractor_point_last(idmc_attractor_point* head);

idmc_attractor* idmc_attractor_new(int dim);
void idmc_attractor_free(idmc_attractor* p);
int idmc_attractor_length(idmc_attractor* p);
/*returns true if point lies on the attractor*/
int idmc_attractor_check_point(idmc_attractor* p, double* x, double eps);
void idmc_attractor_hd_set(idmc_attractor* p, idmc_attractor_point* head);

void idmc_attractor_list_free(idmc_attractor* head);
idmc_attractor* idmc_attractor_list_add(idmc_attractor* last, idmc_attractor* i);
int idmc_attractor_list_length(idmc_attractor* head);
/*return the index of the first attractor in the list containing point 'x'*/
int idmc_attractor_list_check_point(idmc_attractor* head, double* x, double eps);
idmc_attractor* idmc_attractor_list_get(idmc_attractor* head, int id);
idmc_attractor* idmc_attractor_list_last(idmc_attractor* head);
void idmc_attractor_list_drop(idmc_attractor* p);
void idmc_attractor_list_append(idmc_attractor* head, idmc_attractor* i);
/*merge 2 attractors into 1 attractor*/
void idmc_attractor_list_merge(idmc_attractor* head, int a, int b);

#ifdef __cplusplus
}
#endif

#endif
