//
//  closure.h
//  Closures
//
//  Created by João Costa Seco on 5/9/13.
//
//

#ifndef Closures_closure_h
#define Closures_closure_h

/* Var type */
typedef enum {INT,VAR} content_type;

typedef struct var_type {
    content_type type;
    union {
        struct var_type* var_value;
        int integer_value;
    } content;
} var_type;

var_type* int_var_create(int v) ;

var_type* var_var_create(var_type* v) ;

void int_set_var(var_type* var, int value) ;

void var_set_var(var_type* var, var_type* value) ;

int int_get_var(var_type* var) ;

var_type* var_get_var(var_type* var);

#endif
