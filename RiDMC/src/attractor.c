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
#include "attractor.h"

idmc_attractor_point *idmc_attractor_point_new(double* x, int n) {
	idmc_attractor_point *ans = (idmc_attractor_point*) malloc(sizeof(idmc_attractor_point));
	ans->x = (double*) malloc(n * sizeof(double));
	memcpy(ans->x, x, n * sizeof(double));
	ans->next = NULL;
	return ans;
}

void idmc_attractor_point_free(idmc_attractor_point* p) {
	idmc_attractor_point *n;
	while(p) {
		n = p->next;
		free(p->x);
		free(p);
		p = n;
	}
}

idmc_attractor_point* idmc_attractor_point_add(idmc_attractor_point* last,
 idmc_attractor_point* p) {
	last->next = p;
	return p;
}

idmc_attractor_point* idmc_attractor_point_last(idmc_attractor_point* head) {
	while(head->next)
		head = head->next;
	return head;
}

idmc_attractor_point* idmc_attractor_point_clone(idmc_attractor_point* head, int dim) {
	idmc_attractor_point* ans = idmc_attractor_point_new(head->x, dim);
	idmc_attractor_point* p = ans;
	while(head->next) {
		head = head->next;
		p->next = idmc_attractor_point_new(head->x, dim);
		p = p->next;
	}
	return ans;
}

idmc_attractor* idmc_attractor_new(int dim) {
	idmc_attractor* ans = (idmc_attractor*) malloc(sizeof(idmc_attractor));
	ans->hd = NULL;
	ans->dim = dim;
	ans->next = NULL;
	ans->previous = NULL;
	return ans;
}

void idmc_attractor_free(idmc_attractor* p) {
	idmc_attractor_point_free(p->hd);
	free(p);
}

int idmc_attractor_length(idmc_attractor* p) {
	int i=0;
	idmc_attractor_point* hd = p->hd;
	while(hd) {
		i++;
		hd = hd->next;
	}
	return i;
}

static int check_points(double *a, double *b, int dim, double eps) {
	int i;
	double dst = 0.0;
	for(i=0; i<dim; i++)
		dst += (a[i] - b[i])*(a[i] - b[i]);
	return ( dst < eps );
}

/*returns true if point lies on the attractor*/
int idmc_attractor_check_point(idmc_attractor* p, double* x, double eps) {
	idmc_attractor_point *ap = p->hd;
	while(ap) {
		if(check_points(ap->x, x, p->dim, eps))
			return 1;
		ap = ap->next;
	}
	return 0;
}

void idmc_attractor_hd_set(idmc_attractor* p, idmc_attractor_point* head) {
	p->hd = head;
}
