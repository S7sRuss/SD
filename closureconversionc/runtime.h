//
//  runtime.h
//  runtime closures
//
//  Created by João Costa Seco on 5/15/13.
//  Copyright (c) 2013 João Costa Seco. All rights reserved.
//

#ifndef Closures_closure_h
#define Closures_closure_h

/* Var type */
typedef enum {INT,VAR,CLOSURE} content_type;

typedef struct closure_type {
    void *env;
    void (*f)(void);
} closure_type;

typedef struct var_type {
    content_type type;
    union {
        struct var_type* var_value;
        int integer_value;
        closure_type* closure_value;
    } content;
} var_type;

var_type* int_var_create(int v) ;

var_type* var_var_create(var_type* v) ;

void int_set_var(var_type* var, int value) ;

void var_set_var(var_type* var, var_type* value) ;

int int_get_var(var_type* var) ;

var_type* var_get_var(var_type* var);


closure_type* closure_create(int env_size, void (*f)(void));

var_type* closure_var_create(closure_type* c);

#endif
