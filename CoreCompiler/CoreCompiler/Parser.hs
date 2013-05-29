{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser where

import Lexer
import Syntax
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (15#) = happyShift action_10
action_0 (20#) = happyShift action_11
action_0 (22#) = happyShift action_12
action_0 (25#) = happyShift action_13
action_0 (30#) = happyShift action_14
action_0 (34#) = happyShift action_15
action_0 (36#) = happyShift action_16
action_0 (37#) = happyShift action_17
action_0 (38#) = happyShift action_18
action_0 (41#) = happyShift action_19
action_0 (42#) = happyShift action_20
action_0 (4#) = happyGoto action_21
action_0 (5#) = happyGoto action_2
action_0 (6#) = happyGoto action_3
action_0 (9#) = happyGoto action_4
action_0 (10#) = happyGoto action_5
action_0 (11#) = happyGoto action_6
action_0 (12#) = happyGoto action_7
action_0 (13#) = happyGoto action_8
action_0 (14#) = happyGoto action_9
action_0 x = happyTcHack x happyFail

action_1 (15#) = happyShift action_10
action_1 (20#) = happyShift action_11
action_1 (22#) = happyShift action_12
action_1 (25#) = happyShift action_13
action_1 (30#) = happyShift action_14
action_1 (34#) = happyShift action_15
action_1 (36#) = happyShift action_16
action_1 (37#) = happyShift action_17
action_1 (38#) = happyShift action_18
action_1 (41#) = happyShift action_19
action_1 (42#) = happyShift action_20
action_1 (5#) = happyGoto action_2
action_1 (6#) = happyGoto action_3
action_1 (9#) = happyGoto action_4
action_1 (10#) = happyGoto action_5
action_1 (11#) = happyGoto action_6
action_1 (12#) = happyGoto action_7
action_1 (13#) = happyGoto action_8
action_1 (14#) = happyGoto action_9
action_1 x = happyTcHack x happyFail

action_2 (45#) = happyShift action_39
action_2 x = happyTcHack x happyFail

action_3 (44#) = happyShift action_38
action_3 x = happyTcHack x happyReduce_2

action_4 (24#) = happyShift action_36
action_4 (35#) = happyShift action_37
action_4 x = happyTcHack x happyReduce_8

action_5 (16#) = happyShift action_34
action_5 (17#) = happyShift action_35
action_5 x = happyTcHack x happyReduce_16

action_6 (18#) = happyShift action_32
action_6 (19#) = happyShift action_33
action_6 x = happyTcHack x happyReduce_20

action_7 (15#) = happyShift action_10
action_7 (20#) = happyShift action_11
action_7 (25#) = happyShift action_13
action_7 (34#) = happyShift action_15
action_7 (36#) = happyShift action_16
action_7 (37#) = happyShift action_17
action_7 (41#) = happyShift action_19
action_7 (13#) = happyGoto action_31
action_7 (14#) = happyGoto action_9
action_7 x = happyTcHack x happyReduce_23

action_8 x = happyTcHack x happyReduce_24

action_9 x = happyTcHack x happyReduce_26

action_10 x = happyTcHack x happyReduce_29

action_11 (15#) = happyShift action_10
action_11 (20#) = happyShift action_11
action_11 (22#) = happyShift action_12
action_11 (25#) = happyShift action_13
action_11 (30#) = happyShift action_14
action_11 (34#) = happyShift action_15
action_11 (36#) = happyShift action_16
action_11 (37#) = happyShift action_17
action_11 (38#) = happyShift action_18
action_11 (41#) = happyShift action_19
action_11 (42#) = happyShift action_20
action_11 (5#) = happyGoto action_30
action_11 (6#) = happyGoto action_3
action_11 (9#) = happyGoto action_4
action_11 (10#) = happyGoto action_5
action_11 (11#) = happyGoto action_6
action_11 (12#) = happyGoto action_7
action_11 (13#) = happyGoto action_8
action_11 (14#) = happyGoto action_9
action_11 x = happyTcHack x happyFail

action_12 (25#) = happyShift action_29
action_12 x = happyTcHack x happyFail

action_13 x = happyTcHack x happyReduce_31

action_14 (25#) = happyShift action_28
action_14 x = happyTcHack x happyFail

action_15 (15#) = happyShift action_10
action_15 (20#) = happyShift action_11
action_15 (25#) = happyShift action_13
action_15 (36#) = happyShift action_16
action_15 (37#) = happyShift action_17
action_15 (14#) = happyGoto action_27
action_15 x = happyTcHack x happyFail

action_16 (20#) = happyShift action_26
action_16 x = happyTcHack x happyFail

action_17 (20#) = happyShift action_25
action_17 x = happyTcHack x happyFail

action_18 (15#) = happyShift action_10
action_18 (20#) = happyShift action_11
action_18 (22#) = happyShift action_12
action_18 (25#) = happyShift action_13
action_18 (30#) = happyShift action_14
action_18 (34#) = happyShift action_15
action_18 (36#) = happyShift action_16
action_18 (37#) = happyShift action_17
action_18 (38#) = happyShift action_18
action_18 (41#) = happyShift action_19
action_18 (42#) = happyShift action_20
action_18 (5#) = happyGoto action_24
action_18 (6#) = happyGoto action_3
action_18 (9#) = happyGoto action_4
action_18 (10#) = happyGoto action_5
action_18 (11#) = happyGoto action_6
action_18 (12#) = happyGoto action_7
action_18 (13#) = happyGoto action_8
action_18 (14#) = happyGoto action_9
action_18 x = happyTcHack x happyFail

action_19 (15#) = happyShift action_10
action_19 (20#) = happyShift action_11
action_19 (25#) = happyShift action_13
action_19 (36#) = happyShift action_16
action_19 (37#) = happyShift action_17
action_19 (14#) = happyGoto action_23
action_19 x = happyTcHack x happyFail

action_20 (15#) = happyShift action_10
action_20 (20#) = happyShift action_11
action_20 (22#) = happyShift action_12
action_20 (25#) = happyShift action_13
action_20 (30#) = happyShift action_14
action_20 (34#) = happyShift action_15
action_20 (36#) = happyShift action_16
action_20 (37#) = happyShift action_17
action_20 (38#) = happyShift action_18
action_20 (41#) = happyShift action_19
action_20 (42#) = happyShift action_20
action_20 (5#) = happyGoto action_22
action_20 (6#) = happyGoto action_3
action_20 (9#) = happyGoto action_4
action_20 (10#) = happyGoto action_5
action_20 (11#) = happyGoto action_6
action_20 (12#) = happyGoto action_7
action_20 (13#) = happyGoto action_8
action_20 (14#) = happyGoto action_9
action_20 x = happyTcHack x happyFail

action_21 (46#) = happyAccept
action_21 x = happyTcHack x happyFail

action_22 (43#) = happyShift action_53
action_22 x = happyTcHack x happyFail

action_23 x = happyTcHack x happyReduce_28

action_24 (39#) = happyShift action_52
action_24 x = happyTcHack x happyFail

action_25 (15#) = happyShift action_10
action_25 (20#) = happyShift action_11
action_25 (22#) = happyShift action_12
action_25 (25#) = happyShift action_13
action_25 (30#) = happyShift action_14
action_25 (34#) = happyShift action_15
action_25 (36#) = happyShift action_16
action_25 (37#) = happyShift action_17
action_25 (38#) = happyShift action_18
action_25 (41#) = happyShift action_19
action_25 (42#) = happyShift action_20
action_25 (5#) = happyGoto action_51
action_25 (6#) = happyGoto action_3
action_25 (9#) = happyGoto action_4
action_25 (10#) = happyGoto action_5
action_25 (11#) = happyGoto action_6
action_25 (12#) = happyGoto action_7
action_25 (13#) = happyGoto action_8
action_25 (14#) = happyGoto action_9
action_25 x = happyTcHack x happyFail

action_26 (15#) = happyShift action_10
action_26 (20#) = happyShift action_11
action_26 (22#) = happyShift action_12
action_26 (25#) = happyShift action_13
action_26 (30#) = happyShift action_14
action_26 (34#) = happyShift action_15
action_26 (36#) = happyShift action_16
action_26 (37#) = happyShift action_17
action_26 (38#) = happyShift action_18
action_26 (41#) = happyShift action_19
action_26 (42#) = happyShift action_20
action_26 (5#) = happyGoto action_50
action_26 (6#) = happyGoto action_3
action_26 (9#) = happyGoto action_4
action_26 (10#) = happyGoto action_5
action_26 (11#) = happyGoto action_6
action_26 (12#) = happyGoto action_7
action_26 (13#) = happyGoto action_8
action_26 (14#) = happyGoto action_9
action_26 x = happyTcHack x happyFail

action_27 x = happyTcHack x happyReduce_27

action_28 (33#) = happyShift action_49
action_28 x = happyTcHack x happyFail

action_29 (24#) = happyShift action_48
action_29 x = happyTcHack x happyFail

action_30 (21#) = happyShift action_47
action_30 x = happyTcHack x happyFail

action_31 x = happyTcHack x happyReduce_25

action_32 (15#) = happyShift action_10
action_32 (20#) = happyShift action_11
action_32 (25#) = happyShift action_13
action_32 (34#) = happyShift action_15
action_32 (36#) = happyShift action_16
action_32 (37#) = happyShift action_17
action_32 (41#) = happyShift action_19
action_32 (12#) = happyGoto action_46
action_32 (13#) = happyGoto action_8
action_32 (14#) = happyGoto action_9
action_32 x = happyTcHack x happyFail

action_33 (15#) = happyShift action_10
action_33 (20#) = happyShift action_11
action_33 (25#) = happyShift action_13
action_33 (34#) = happyShift action_15
action_33 (36#) = happyShift action_16
action_33 (37#) = happyShift action_17
action_33 (41#) = happyShift action_19
action_33 (12#) = happyGoto action_45
action_33 (13#) = happyGoto action_8
action_33 (14#) = happyGoto action_9
action_33 x = happyTcHack x happyFail

action_34 (15#) = happyShift action_10
action_34 (20#) = happyShift action_11
action_34 (25#) = happyShift action_13
action_34 (34#) = happyShift action_15
action_34 (36#) = happyShift action_16
action_34 (37#) = happyShift action_17
action_34 (41#) = happyShift action_19
action_34 (11#) = happyGoto action_44
action_34 (12#) = happyGoto action_7
action_34 (13#) = happyGoto action_8
action_34 (14#) = happyGoto action_9
action_34 x = happyTcHack x happyFail

action_35 (15#) = happyShift action_10
action_35 (20#) = happyShift action_11
action_35 (25#) = happyShift action_13
action_35 (34#) = happyShift action_15
action_35 (36#) = happyShift action_16
action_35 (37#) = happyShift action_17
action_35 (41#) = happyShift action_19
action_35 (11#) = happyGoto action_43
action_35 (12#) = happyGoto action_7
action_35 (13#) = happyGoto action_8
action_35 (14#) = happyGoto action_9
action_35 x = happyTcHack x happyFail

action_36 (15#) = happyShift action_10
action_36 (20#) = happyShift action_11
action_36 (25#) = happyShift action_13
action_36 (34#) = happyShift action_15
action_36 (36#) = happyShift action_16
action_36 (37#) = happyShift action_17
action_36 (41#) = happyShift action_19
action_36 (10#) = happyGoto action_42
action_36 (11#) = happyGoto action_6
action_36 (12#) = happyGoto action_7
action_36 (13#) = happyGoto action_8
action_36 (14#) = happyGoto action_9
action_36 x = happyTcHack x happyFail

action_37 (15#) = happyShift action_10
action_37 (20#) = happyShift action_11
action_37 (25#) = happyShift action_13
action_37 (34#) = happyShift action_15
action_37 (36#) = happyShift action_16
action_37 (37#) = happyShift action_17
action_37 (41#) = happyShift action_19
action_37 (6#) = happyGoto action_41
action_37 (9#) = happyGoto action_4
action_37 (10#) = happyGoto action_5
action_37 (11#) = happyGoto action_6
action_37 (12#) = happyGoto action_7
action_37 (13#) = happyGoto action_8
action_37 (14#) = happyGoto action_9
action_37 x = happyTcHack x happyFail

action_38 (15#) = happyShift action_10
action_38 (20#) = happyShift action_11
action_38 (22#) = happyShift action_12
action_38 (25#) = happyShift action_13
action_38 (30#) = happyShift action_14
action_38 (34#) = happyShift action_15
action_38 (36#) = happyShift action_16
action_38 (37#) = happyShift action_17
action_38 (38#) = happyShift action_18
action_38 (41#) = happyShift action_19
action_38 (42#) = happyShift action_20
action_38 (5#) = happyGoto action_40
action_38 (6#) = happyGoto action_3
action_38 (9#) = happyGoto action_4
action_38 (10#) = happyGoto action_5
action_38 (11#) = happyGoto action_6
action_38 (12#) = happyGoto action_7
action_38 (13#) = happyGoto action_8
action_38 (14#) = happyGoto action_9
action_38 x = happyTcHack x happyFail

action_39 x = happyTcHack x happyReduce_1

action_40 x = happyTcHack x happyReduce_3

action_41 x = happyTcHack x happyReduce_9

action_42 (16#) = happyShift action_34
action_42 (17#) = happyShift action_35
action_42 x = happyTcHack x happyReduce_17

action_43 (18#) = happyShift action_32
action_43 (19#) = happyShift action_33
action_43 x = happyTcHack x happyReduce_19

action_44 (18#) = happyShift action_32
action_44 (19#) = happyShift action_33
action_44 x = happyTcHack x happyReduce_18

action_45 (15#) = happyShift action_10
action_45 (20#) = happyShift action_11
action_45 (25#) = happyShift action_13
action_45 (34#) = happyShift action_15
action_45 (36#) = happyShift action_16
action_45 (37#) = happyShift action_17
action_45 (41#) = happyShift action_19
action_45 (13#) = happyGoto action_31
action_45 (14#) = happyGoto action_9
action_45 x = happyTcHack x happyReduce_22

action_46 (15#) = happyShift action_10
action_46 (20#) = happyShift action_11
action_46 (25#) = happyShift action_13
action_46 (34#) = happyShift action_15
action_46 (36#) = happyShift action_16
action_46 (37#) = happyShift action_17
action_46 (41#) = happyShift action_19
action_46 (13#) = happyGoto action_31
action_46 (14#) = happyGoto action_9
action_46 x = happyTcHack x happyReduce_21

action_47 x = happyTcHack x happyReduce_30

action_48 (15#) = happyShift action_10
action_48 (20#) = happyShift action_11
action_48 (22#) = happyShift action_12
action_48 (25#) = happyShift action_13
action_48 (30#) = happyShift action_14
action_48 (34#) = happyShift action_15
action_48 (36#) = happyShift action_16
action_48 (37#) = happyShift action_17
action_48 (38#) = happyShift action_18
action_48 (41#) = happyShift action_19
action_48 (42#) = happyShift action_20
action_48 (5#) = happyGoto action_64
action_48 (6#) = happyGoto action_3
action_48 (9#) = happyGoto action_4
action_48 (10#) = happyGoto action_5
action_48 (11#) = happyGoto action_6
action_48 (12#) = happyGoto action_7
action_48 (13#) = happyGoto action_8
action_48 (14#) = happyGoto action_9
action_48 x = happyTcHack x happyFail

action_49 (26#) = happyShift action_60
action_49 (27#) = happyShift action_61
action_49 (28#) = happyShift action_62
action_49 (29#) = happyShift action_63
action_49 (7#) = happyGoto action_58
action_49 (8#) = happyGoto action_59
action_49 x = happyTcHack x happyFail

action_50 (21#) = happyShift action_57
action_50 x = happyTcHack x happyFail

action_51 (21#) = happyShift action_56
action_51 x = happyTcHack x happyFail

action_52 (15#) = happyShift action_10
action_52 (20#) = happyShift action_11
action_52 (22#) = happyShift action_12
action_52 (25#) = happyShift action_13
action_52 (30#) = happyShift action_14
action_52 (34#) = happyShift action_15
action_52 (36#) = happyShift action_16
action_52 (37#) = happyShift action_17
action_52 (38#) = happyShift action_18
action_52 (41#) = happyShift action_19
action_52 (42#) = happyShift action_20
action_52 (5#) = happyGoto action_55
action_52 (6#) = happyGoto action_3
action_52 (9#) = happyGoto action_4
action_52 (10#) = happyGoto action_5
action_52 (11#) = happyGoto action_6
action_52 (12#) = happyGoto action_7
action_52 (13#) = happyGoto action_8
action_52 (14#) = happyGoto action_9
action_52 x = happyTcHack x happyFail

action_53 (15#) = happyShift action_10
action_53 (20#) = happyShift action_11
action_53 (22#) = happyShift action_12
action_53 (25#) = happyShift action_13
action_53 (30#) = happyShift action_14
action_53 (34#) = happyShift action_15
action_53 (36#) = happyShift action_16
action_53 (37#) = happyShift action_17
action_53 (38#) = happyShift action_18
action_53 (41#) = happyShift action_19
action_53 (42#) = happyShift action_20
action_53 (5#) = happyGoto action_54
action_53 (6#) = happyGoto action_3
action_53 (9#) = happyGoto action_4
action_53 (10#) = happyGoto action_5
action_53 (11#) = happyGoto action_6
action_53 (12#) = happyGoto action_7
action_53 (13#) = happyGoto action_8
action_53 (14#) = happyGoto action_9
action_53 x = happyTcHack x happyFail

action_54 x = happyTcHack x happyReduce_7

action_55 (40#) = happyShift action_69
action_55 x = happyTcHack x happyFail

action_56 x = happyTcHack x happyReduce_32

action_57 x = happyTcHack x happyReduce_33

action_58 (31#) = happyShift action_68
action_58 x = happyTcHack x happyFail

action_59 (32#) = happyShift action_67
action_59 x = happyTcHack x happyReduce_10

action_60 x = happyTcHack x happyReduce_12

action_61 x = happyTcHack x happyReduce_13

action_62 (20#) = happyShift action_66
action_62 x = happyTcHack x happyFail

action_63 x = happyTcHack x happyReduce_15

action_64 (23#) = happyShift action_65
action_64 x = happyTcHack x happyFail

action_65 (15#) = happyShift action_10
action_65 (20#) = happyShift action_11
action_65 (22#) = happyShift action_12
action_65 (25#) = happyShift action_13
action_65 (30#) = happyShift action_14
action_65 (34#) = happyShift action_15
action_65 (36#) = happyShift action_16
action_65 (37#) = happyShift action_17
action_65 (38#) = happyShift action_18
action_65 (41#) = happyShift action_19
action_65 (42#) = happyShift action_20
action_65 (5#) = happyGoto action_74
action_65 (6#) = happyGoto action_3
action_65 (9#) = happyGoto action_4
action_65 (10#) = happyGoto action_5
action_65 (11#) = happyGoto action_6
action_65 (12#) = happyGoto action_7
action_65 (13#) = happyGoto action_8
action_65 (14#) = happyGoto action_9
action_65 x = happyTcHack x happyFail

action_66 (26#) = happyShift action_60
action_66 (27#) = happyShift action_61
action_66 (28#) = happyShift action_62
action_66 (29#) = happyShift action_63
action_66 (7#) = happyGoto action_73
action_66 (8#) = happyGoto action_59
action_66 x = happyTcHack x happyFail

action_67 (26#) = happyShift action_60
action_67 (27#) = happyShift action_61
action_67 (28#) = happyShift action_62
action_67 (29#) = happyShift action_63
action_67 (7#) = happyGoto action_72
action_67 (8#) = happyGoto action_59
action_67 x = happyTcHack x happyFail

action_68 (15#) = happyShift action_10
action_68 (20#) = happyShift action_11
action_68 (22#) = happyShift action_12
action_68 (25#) = happyShift action_13
action_68 (30#) = happyShift action_14
action_68 (34#) = happyShift action_15
action_68 (36#) = happyShift action_16
action_68 (37#) = happyShift action_17
action_68 (38#) = happyShift action_18
action_68 (41#) = happyShift action_19
action_68 (42#) = happyShift action_20
action_68 (5#) = happyGoto action_71
action_68 (6#) = happyGoto action_3
action_68 (9#) = happyGoto action_4
action_68 (10#) = happyGoto action_5
action_68 (11#) = happyGoto action_6
action_68 (12#) = happyGoto action_7
action_68 (13#) = happyGoto action_8
action_68 (14#) = happyGoto action_9
action_68 x = happyTcHack x happyFail

action_69 (15#) = happyShift action_10
action_69 (20#) = happyShift action_11
action_69 (22#) = happyShift action_12
action_69 (25#) = happyShift action_13
action_69 (30#) = happyShift action_14
action_69 (34#) = happyShift action_15
action_69 (36#) = happyShift action_16
action_69 (37#) = happyShift action_17
action_69 (38#) = happyShift action_18
action_69 (41#) = happyShift action_19
action_69 (42#) = happyShift action_20
action_69 (5#) = happyGoto action_70
action_69 (6#) = happyGoto action_3
action_69 (9#) = happyGoto action_4
action_69 (10#) = happyGoto action_5
action_69 (11#) = happyGoto action_6
action_69 (12#) = happyGoto action_7
action_69 (13#) = happyGoto action_8
action_69 (14#) = happyGoto action_9
action_69 x = happyTcHack x happyFail

action_70 x = happyTcHack x happyReduce_6

action_71 x = happyTcHack x happyReduce_5

action_72 x = happyTcHack x happyReduce_11

action_73 (21#) = happyShift action_75
action_73 x = happyTcHack x happyFail

action_74 x = happyTcHack x happyReduce_4

action_75 x = happyTcHack x happyReduce_14

happyReduce_1 = happySpecReduce_2  4# happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5# happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5# happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 6# 5# happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Decl happy_var_2 happy_var_4 happy_var_6 None
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6# 5# happyReduction_5
happyReduction_5 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Fun happy_var_2 happy_var_4 happy_var_6 None
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6# 5# happyReduction_6
happyReduction_6 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6 None
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4# 5# happyReduction_7
happyReduction_7 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  6# happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6# happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (Assign happy_var_1 happy_var_3 None
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7# happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (FunType happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8# happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn8
		 (IntType
	)

happyReduce_13 = happySpecReduce_1  8# happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn8
		 (BoolType
	)

happyReduce_14 = happyReduce 4# 8# happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (RefType happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  8# happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (UnitType
	)

happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9# happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Equal happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10# happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Add happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10# happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10# happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11# happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11# happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Div happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  11# happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12# happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  12# happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Call happy_var_1 happy_var_2 None
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13# happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  13# happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Deref happy_var_2 None
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  13# happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Not happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  14# happyReduction_29
happyReduction_29 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn14
		 (Num happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  14# happyReduction_30
happyReduction_30 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  14# happyReduction_31
happyReduction_31 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn14
		 (Id happy_var_1 None
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 4# 14# happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Var happy_var_3 None
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 4# 14# happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Free happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 46# 46# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 15#;
	TokenPlus -> cont 16#;
	TokenMinus -> cont 17#;
	TokenMul -> cont 18#;
	TokenDiv -> cont 19#;
	TokenLParen -> cont 20#;
	TokenRParen -> cont 21#;
	TokenDecl -> cont 22#;
	TokenIn -> cont 23#;
	TokenEq -> cont 24#;
	TokenId happy_dollar_dollar -> cont 25#;
	TokenInt -> cont 26#;
	TokenBool -> cont 27#;
	TokenRef -> cont 28#;
	TokenUnit -> cont 29#;
	TokenFun -> cont 30#;
	TokenArrow -> cont 31#;
	TokenDArrow -> cont 32#;
	TokenColon -> cont 33#;
	TokenDeref -> cont 34#;
	TokenAssign -> cont 35#;
	TokenFree -> cont 36#;
	TokenVar -> cont 37#;
	TokenIf -> cont 38#;
	TokenThen -> cont 39#;
	TokenElse -> cont 40#;
	TokenNot -> cont 41#;
	TokenWhile -> cont 42#;
	TokenDo -> cont 43#;
	TokenSeq -> cont 44#;
	TokenEnd -> cont 45#;
	_ -> happyError' (tk:tks)
	}

happyError_ 46# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<inbyggd>" #-}
{-# LINE 1 "<kommandorad>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates\\GenericTemplate.hs" #-}








{-# LINE 51 "templates\\GenericTemplate.hs" #-}

{-# LINE 61 "templates\\GenericTemplate.hs" #-}

{-# LINE 70 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 1# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
