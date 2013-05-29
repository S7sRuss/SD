//
//  runtime.c
//  runtime closures
//
//  Created by João Costa Seco on 5/15/13.
//  Copyright (c) 2013 João Costa Seco. All rights reserved.
//

#include <libc.h>
#include "runtime.h"

var_type* var_create() {
    var_type* var = malloc(sizeof(var_type));
    return var;
}

var_type* int_var_create(int v) {
    var_type* var = var_create();
    var->type = INT;
    var->content.integer_value = v;
    return var;
}

var_type* var_var_create(var_type* v) {
    var_type* var = var_create();
    var->type = VAR;
    var->content.var_value = v;
    return var;
}

void int_set_var(var_type* var, int value) {
    var->content.integer_value = value;
}

void var_set_var(var_type* var, var_type* value) {
    var->content.var_value = value;
}

int int_get_var(var_type* var) {
    return var->content.integer_value;
}

var_type* var_get_var(var_type* var) {
    return var->content.var_value;
}




closure_type* closure_create(int env_size, void (*f)(void)) {
    closure_type *closure = malloc(sizeof(closure_type));
    closure->env = malloc(sizeof(env_size));
    closure->f = f;
    return closure;
    
}

var_type* closure_var_create(closure_type* c) {
    var_type* var = var_create();
    var->type = CLOSURE;
    var->content.closure_value = c;
    return var;
}




/* Particular code for each compiled program */


/* f0 closure */

/* Function type type */
typedef struct int_int_type {
    void *env;
    int (*f)(void*env, int x);
} int_int_type;

/* f0 env */
typedef struct f0_env {
    int x;
    int z;
} f0_env;

/* f0 value type */
int f0_code(void* _env, int y) {
    f0_env *env = (f0_env*) _env;
    return env->x + y + env->z;
}

/* f0 create function, allocates memory */
closure_type* f0_create(int x, int z) {
    closure_type* closure = closure_create(sizeof(f0_env), (void (*)(void))&f0_code);
    ((f0_env*)closure->env)->x = x;
    ((f0_env*)closure->env)->z = z;
    return closure;
}


