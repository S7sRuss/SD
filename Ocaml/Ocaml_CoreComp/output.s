@.str = private constant [4 x i8] c"%d\0A\00", align 1 ; <[4 x i8]*> [#uses=1]

%struct.var_type = type { i32, %union.anon }
%union.anon = type { %struct.var_type* }

define i32 @main() nounwind {
entry:
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i64 0, i64 0), i32 1) nounwind ; <i32> [#uses=0]
  ret i32 0
}

declare i32 @printf(i8*, ...) nounwind
declare %struct.var_type* @int_var_create(i32)
declare %struct.var_type* @var_var_create(%struct.var_type*)
declare i32 @int_get_var(%struct.var_type*)
declare %struct.var_type* @var_get_var(%struct.var_type*)
declare void @int_set_var(%struct.var_type*, i32)
declare void @var_set_var(%struct.var_type*, %struct.var_type*)
declare void @free_var(%struct.var_type*, %struct.var_type*)
