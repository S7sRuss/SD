//
//  main.c
//  runtime
//
//  Created by João Costa Seco on 5/9/13.
//  Copyright (c) 2013 João Costa Seco. All rights reserved.
//

#include <stdio.h>
#include "runtime.h"

int testVar() {
    int x = 1;
    var_type *v = int_var_create(x);
    int_set_var(v,98);
    
    printf("%d\n",int_get_var(v));
    
    var_type *v2 = var_var_create(v);
    var_set_var(v2,v);
    
    printf("%d\n",int_get_var(var_get_var(v2)));
    
    return 0;
}

int main() {
    testVar();
}
