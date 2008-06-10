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

void idmc_attractor_list_free(idmc_attractor* head) {
	idmc_attractor *n;
	while(head) {
		n = head->next;
		idmc_attractor_free(head);
		head = n;
	}
}

idmc_attractor* idmc_attractor_list_add(idmc_attractor* last, idmc_attractor* i) {
	i->previous = last;
	last->next = i;
	return i;
}

int idmc_attractor_list_length(idmc_attractor* head) {
	int i=0;
	while(head) {
		i++;
		head = head->next;
	}
	return i;
}

/*return the index of the first attractor in the list containing point 'x'*/
int idmc_attractor_list_check_point(idmc_attractor* head, double* x, double eps) {
	int i=0;
	while(head) {
		if( idmc_attractor_check_point(head, x, eps) )
			return i;
		head = head->next;
		i++;
	}
	return i;
}

idmc_attractor* idmc_attractor_list_get(idmc_attractor* head, int id) {
	int i;
	idmc_attractor* ans = head;
	for(i=0; i<id; i++)
		ans = ans->next;
	return ans;
}

idmc_attractor* idmc_attractor_list_last(idmc_attractor* head) {
	while(head->next)
		head = head->next;
	return head;
}

void idmc_attractor_list_append(idmc_attractor* head, idmc_attractor* i) {
	idmc_attractor* last = idmc_attractor_list_last(head);
	last->next = i;
	i->previous = last;
}

void idmc_attractor_list_drop(idmc_attractor* p) {
	if(p->previous)
		(p->previous)->next = p->next;
	if(p->next)
		(p->next)->previous = p->previous;
	idmc_attractor_free(p);
}

/*merge 2 attractors into 1 attractor*/
void idmc_attractor_list_merge(idmc_attractor* head, int a, int b) {
	int tmp;
	if(a>b) {
		tmp=a;
		a=b;
		b=tmp;
	}
	idmc_attractor* a1 = idmc_attractor_list_get(head, a);
	idmc_attractor* a2 = idmc_attractor_list_get(head, b);
	idmc_attractor_point* tail1 = idmc_attractor_point_last( a1->hd );
	idmc_attractor_point* head2 = a2->hd;
	tail1->next = idmc_attractor_point_clone(head2, head->dim);
	idmc_attractor_list_drop(a2);
}
