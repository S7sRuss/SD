//
//  runtime.c
//  runtime support
//
//  Created by João Costa Seco on 5/22/13.
//  Copyright (c) 2013 João Costa Seco. All rights reserved.
//

#include <libc.h>
#include "runtime.h"

var_type* var_create() {
    var_type* var = malloc(sizeof(var_type));
    return var;
}

void free_var(var_type* v) {
    free(v);
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

closure_type* closure_create(void* env, void (*f)(void)) {
    closure_type *closure = malloc(sizeof(closure_type));
    closure->env = env;
    closure->f = f;
    return closure;
}


var_type* closure_var_create(closure_type* c) {
    var_type* var = var_create();
    var->type = CLOSURE;
    var->content.closure_value = c;
    return var;
}
