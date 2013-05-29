//
//  main.c
//  closures
//
//  Created by João Costa Seco on 5/16/13.
//  Copyright (c) 2013 João Costa Seco. All rights reserved.
//

#include <stdio.h>
//#include "runtime.h"
#include <stdlib.h>


/*
 
 decl f = fun x -> x+1 in
 decl g = fun y -> y+f(y) in
 g (2)
 
 */


int f(int x) {
    return x+1;
}

int g(int y) {
    return y+f(y);
}

int main0() {
    printf("%d\n", g(2));
    return 0;
}


/**

 decl h = 
    decl x = 1 in fun y -> x+y in
 decl x = 3 in h(x)
 */


/*
typedef struct { // Nomes livres de h - resumo da stack
    int x;
} env_h;

typedef struct {
    int (*fun)(int,env_h*);
    env_h* env;
} closure_type_h;

int h(int y, env_h* env) {
    return env->x+y;
}

closure_type_h* make() {
    
    int x1 = 1;
    closure_type_h* ch = (closure_type_h*) malloc(sizeof(closure_type_h));
    ch->fun = &h;
    ch->env = malloc(sizeof(env_h));
    ch->env->x = x1;
    return ch;
    
}

int main() {
    
    closure_type_h* h = make();

    int x2 = 3;
    printf("%d\n",h->fun(x2,h->env));
}

*/





/*
 
 decl f = (fun x -> fun y -> x + y) in
 decl g = f 0 in 
 g 1
 
 */

typedef struct {
    void* env;
    int (*fun)(int,void*);
} closure_int_int;

typedef struct {
    void* env;
    closure_int_int* (*fun)(int,void*);
} closure_int_fun_int_int;


typedef struct {
    int x;
} env_f1;

int f1(int y, void* ptr) {
    env_f1* env = (env_f1*)ptr;
    return env->x+y; // x+y
}

closure_int_int* f0(int x, void*ptr) {
    closure_int_int* c = malloc(sizeof(closure_int_int));
    c->fun = &f1;
    c->env = malloc(sizeof(env_f1));
    ((env_f1*)c->env)->x = x;
    return c;
}

int main() {
    closure_int_fun_int_int* f = malloc(sizeof(closure_int_fun_int_int));
    f->fun = &f0;
    f->env = NULL;

    closure_int_int* g = f->fun(2,f->env);
    
    int result = g->fun(3,g->env);
    printf("%d\n", result);
}










/**
 decl x = 1 in
 decl f = fun y -> y+x in
 decl g = decl z = 2 in decl y = 3 in fun w -> z+y+w in
 decl h = fun f -> f 1 in
 (h f) + (h g)
 **/

/*
typedef struct {
    int (*fun)(int,void*);
    void* env;
} closure_type_int_int;


typedef struct {
    int (*fun)(closure_type_int_int*,void*);
    void* env;
} closure_type_fun_int_int_int;


typedef struct { 
    int x;
} env_f;

int f(int y, void* ptr) {
    env_f* env = (env_f*) ptr;
    return y+env->x;
}

typedef struct {
    int z;
    int y;
} env_g;

int g(int w, void* ptr) {
    env_g* env = (env_g*) ptr;
    return env->z + env->y + w;
}

int h( closure_type_int_int* f, void* env) {
    return f->fun(1,f->env);
}

int main() {
    
    int x = 1;
    
    closure_type_int_int* cf = (closure_type_int_int*)malloc(sizeof(closure_type_int_int));
    
    cf->fun = &f;
    cf->env = malloc(sizeof(env_f));
    ((env_f*)cf->env)->x = x;
    
    closure_type_int_int* cg = (closure_type_int_int*)malloc(sizeof(closure_type_int_int));
    
    int z = 2;
    int y = 3;
    cg->fun = &g;
    cg->env = (env_g*)malloc(sizeof(env_g));
    ((env_g*)cg->env)->z = z;
    ((env_g*)cg->env)->y = y;
    
    closure_type_fun_int_int_int* ch = (closure_type_fun_int_int_int*)malloc(sizeof(closure_type_fun_int_int_int));
    ch->fun = &h;
    ch->env = NULL;
    
    int result = ch->fun(cf,ch->env) + ch->fun(cg,ch->env);
    
    printf("%d\n", result);
    return 0;
}
*/
