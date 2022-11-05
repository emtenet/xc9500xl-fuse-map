-module(xc9572xl_input_map).

-export([choices/0]).
-export([choice/2]).
-export([sources/2]).

-type input() :: input:input().
-type macro_cell() :: macro_cell:macro_cell().

-type choice() :: non_neg_integer().
-type direction() :: input | output.

%%====================================================================
%% choices
%%====================================================================

-spec choices() -> [choice()].

choices() ->
    [1,2,3,4,16,17,18,19].

%%====================================================================
%% choice
%%====================================================================

-spec choice(input(), choice())
    -> {macro_cell(), direction()} | unknown.

choice(input01,  1) -> {mc02_03,input};
choice(input01,  2) -> {mc02_07,input};
choice(input01,  3) -> {mc01_05,input};
choice(input01,  4) -> {mc01_14,input};
choice(input01, 16) -> {mc01_01,output};
choice(input01, 17) -> {mc03_01,output};
choice(input01, 18) -> {mc02_01,output};
choice(input01, 19) -> {mc04_01,output};
choice(input02,  1) -> {mc04_17,input};
choice(input02,  2) -> {mc04_07,input};
choice(input02,  3) -> {mc04_01,input};
choice(input02,  4) -> {mc03_07,input};
choice(input02, 16) -> {mc01_02,output};
choice(input02, 17) -> {mc03_02,output};
choice(input02, 18) -> {mc02_02,output};
choice(input02, 19) -> {mc04_02,output};
choice(input03,  1) -> {mc02_18,input};
choice(input03,  2) -> {mc02_11,input};
choice(input03,  3) -> {mc01_06,input};
choice(input03,  4) -> {mc01_10,input};
choice(input03, 16) -> {mc01_03,output};
choice(input03, 17) -> {mc03_03,output};
choice(input03, 18) -> {mc02_03,output};
choice(input03, 19) -> {mc04_03,output};
choice(input04,  1) -> {mc04_15,input};
choice(input04,  2) -> {mc04_06,input};
choice(input04,  3) -> {mc03_16,input};
choice(input04,  4) -> {mc03_06,input};
choice(input04, 16) -> {mc01_04,output};
choice(input04, 17) -> {mc03_04,output};
choice(input04, 18) -> {mc02_04,output};
choice(input04, 19) -> {mc04_04,output};
choice(input05,  1) -> {mc02_04,input};
choice(input05,  2) -> {mc02_12,input};
choice(input05,  3) -> {mc01_01,input};
choice(input05,  4) -> {mc01_15,input};
choice(input05, 16) -> {mc01_05,output};
choice(input05, 17) -> {mc03_05,output};
choice(input05, 18) -> {mc02_05,output};
choice(input05, 19) -> {mc04_05,output};
choice(input06,  1) -> {mc02_01,input};
choice(input06,  2) -> {mc04_11,input};
choice(input06,  3) -> {mc03_13,input};
choice(input06,  4) -> {mc03_11,input};
choice(input06, 16) -> {mc01_06,output};
choice(input06, 17) -> {mc03_06,output};
choice(input06, 18) -> {mc02_06,output};
choice(input06, 19) -> {mc04_06,output};
choice(input07,  1) -> {mc02_02,input};
choice(input07,  2) -> {mc02_13,input};
choice(input07,  3) -> {mc01_08,input};
choice(input07,  4) -> {mc01_17,input};
choice(input07, 16) -> {mc01_07,output};
choice(input07, 17) -> {mc03_07,output};
choice(input07, 18) -> {mc02_07,output};
choice(input07, 19) -> {mc04_07,output};
choice(input08,  1) -> {mc04_16,input};
choice(input08,  2) -> {mc04_04,input};
choice(input08,  3) -> {mc03_12,input};
choice(input08,  4) -> {mc03_04,input};
choice(input08, 16) -> {mc01_08,output};
choice(input08, 17) -> {mc03_08,output};
choice(input08, 18) -> {mc02_08,output};
choice(input08, 19) -> {mc04_08,output};
choice(input09,  1) -> {mc02_05,input};
choice(input09,  2) -> {mc02_14,input};
choice(input09,  3) -> {mc01_03,input};
choice(input09,  4) -> {mc03_02,input};
choice(input09, 16) -> {mc01_09,output};
choice(input09, 17) -> {mc03_09,output};
choice(input09, 18) -> {mc02_09,output};
choice(input09, 19) -> {mc04_09,output};
choice(input10,  1) -> {mc04_13,input};
choice(input10,  2) -> {mc04_03,input};
choice(input10,  3) -> {mc03_10,input};
choice(input10,  4) -> {mc03_03,input};
choice(input10, 16) -> {mc01_10,output};
choice(input10, 17) -> {mc03_10,output};
choice(input10, 18) -> {mc02_10,output};
choice(input10, 19) -> {mc04_10,output};
choice(input11,  1) -> {mc02_06,input};
choice(input11,  2) -> {mc02_16,input};
choice(input11,  3) -> {mc01_04,input};
choice(input11,  4) -> {mc01_12,input};
choice(input11, 16) -> {mc01_11,output};
choice(input11, 17) -> {mc03_11,output};
choice(input11, 18) -> {mc02_11,output};
choice(input11, 19) -> {mc04_11,output};
choice(input12,  1) -> {mc04_12,input};
choice(input12,  2) -> {mc04_08,input};
choice(input12,  3) -> {mc03_18,input};
choice(input12,  4) -> {mc03_09,input};
choice(input12, 16) -> {mc01_12,output};
choice(input12, 17) -> {mc03_12,output};
choice(input12, 18) -> {mc02_12,output};
choice(input12, 19) -> {mc04_12,output};
choice(input13,  1) -> {mc02_08,input};
choice(input13,  2) -> {mc02_15,input};
choice(input13,  3) -> {mc01_09,input};
choice(input13,  4) -> {mc03_05,input};
choice(input13, 16) -> {mc01_13,output};
choice(input13, 17) -> {mc03_13,output};
choice(input13, 18) -> {mc02_13,output};
choice(input13, 19) -> {mc04_13,output};
choice(input14,  1) -> {mc04_10,input};
choice(input14,  2) -> {mc04_05,input};
choice(input14,  3) -> {mc03_17,input};
choice(input14,  4) -> {mc03_01,input};
choice(input14, 16) -> {mc01_14,output};
choice(input14, 17) -> {mc03_14,output};
choice(input14, 18) -> {mc02_14,output};
choice(input14, 19) -> {mc04_14,output};
choice(input15,  1) -> {mc02_09,input};
choice(input15,  2) -> {mc02_17,input};
choice(input15,  3) -> {mc01_11,input};
choice(input15,  4) -> {mc01_13,input};
choice(input15, 16) -> {mc01_15,output};
choice(input15, 17) -> {mc03_15,output};
choice(input15, 18) -> {mc02_15,output};
choice(input15, 19) -> {mc04_15,output};
choice(input16,  1) -> {mc04_18,input};
choice(input16,  2) -> {mc04_02,input};
choice(input16,  3) -> {mc03_15,input};
choice(input16,  4) -> {mc01_18,input};
choice(input16, 16) -> {mc01_16,output};
choice(input16, 17) -> {mc03_16,output};
choice(input16, 18) -> {mc02_16,output};
choice(input16, 19) -> {mc04_16,output};
choice(input17,  1) -> {mc02_10,input};
choice(input17,  2) -> {mc01_02,input};
choice(input17,  3) -> {mc01_07,input};
choice(input17,  4) -> {mc03_08,input};
choice(input17, 16) -> {mc01_17,output};
choice(input17, 17) -> {mc03_17,output};
choice(input17, 18) -> {mc02_17,output};
choice(input17, 19) -> {mc04_17,output};
choice(input18,  1) -> {mc04_14,input};
choice(input18,  2) -> {mc04_09,input};
choice(input18,  3) -> {mc03_14,input};
choice(input18,  4) -> {mc01_16,input};
choice(input18, 16) -> {mc01_18,output};
choice(input18, 17) -> {mc03_18,output};
choice(input18, 18) -> {mc02_18,output};
choice(input18, 19) -> {mc04_18,output};
choice(input19,  1) -> {mc02_04,input};
choice(input19,  2) -> {mc02_15,input};
choice(input19,  3) -> {mc03_16,input};
choice(input19,  4) -> {mc03_09,input};
choice(input19, 16) -> unknown;
choice(input19, 17) -> unknown;
choice(input19, 18) -> {mc02_04,output};
choice(input19, 19) -> {mc04_02,output};
choice(input20,  1) -> {mc02_02,input};
choice(input20,  2) -> {mc02_17,input};
choice(input20,  3) -> {mc01_08,input};
choice(input20,  4) -> {mc03_01,input};
choice(input20, 16) -> {mc01_15,output};
choice(input20, 17) -> {mc03_08,output};
choice(input20, 18) -> {mc02_14,output};
choice(input20, 19) -> {mc04_06,output};
choice(input21,  1) -> {mc04_10,input};
choice(input21,  2) -> {mc04_06,input};
choice(input21,  3) -> {mc01_05,input};
choice(input21,  4) -> {mc03_03,input};
choice(input21, 16) -> {mc01_11,output};
choice(input21, 17) -> {mc03_15,output};
choice(input21, 18) -> {mc02_05,output};
choice(input21, 19) -> {mc04_03,output};
choice(input22,  1) -> {mc04_12,input};
choice(input22,  2) -> {mc02_16,input};
choice(input22,  3) -> {mc01_04,input};
choice(input22,  4) -> {mc03_05,input};
choice(input22, 16) -> {mc01_06,output};
choice(input22, 17) -> {mc03_04,output};
choice(input22, 18) -> {mc02_11,output};
choice(input22, 19) -> unknown;
choice(input23,  1) -> {mc04_16,input};
choice(input23,  2) -> {mc02_14,input};
choice(input23,  3) -> {mc01_06,input};
choice(input23,  4) -> {mc03_02,input};
choice(input23, 16) -> {mc01_15,output};
choice(input23, 17) -> {mc03_06,output};
choice(input23, 18) -> {mc02_07,output};
choice(input23, 19) -> unknown;
choice(input24,  1) -> {mc02_05,input};
choice(input24,  2) -> {mc04_11,input};
choice(input24,  3) -> {mc04_01,input};
choice(input24,  4) -> {mc01_18,input};
choice(input24, 16) -> {mc01_18,output};
choice(input24, 17) -> {mc03_16,output};
choice(input24, 18) -> {mc02_15,output};
choice(input24, 19) -> {mc04_15,output};
choice(input25,  1) -> {mc02_03,input};
choice(input25,  2) -> {mc04_09,input};
choice(input25,  3) -> {mc03_15,input};
choice(input25,  4) -> {mc01_14,input};
choice(input25, 16) -> {mc01_07,output};
choice(input25, 17) -> unknown;
choice(input25, 18) -> {mc02_08,output};
choice(input25, 19) -> {mc04_08,output};
choice(input26,  1) -> {mc02_06,input};
choice(input26,  2) -> {mc02_13,input};
choice(input26,  3) -> {mc01_04,input};
choice(input26,  4) -> {mc03_08,input};
choice(input26, 16) -> {mc01_04,output};
choice(input26, 17) -> {mc03_14,output};
choice(input26, 18) -> {mc02_03,output};
choice(input26, 19) -> {mc04_14,output};
choice(input27,  1) -> {mc04_13,input};
choice(input27,  2) -> {mc02_07,input};
choice(input27,  3) -> {mc01_03,input};
choice(input27,  4) -> {mc03_08,input};
choice(input27, 16) -> unknown;
choice(input27, 17) -> {mc03_12,output};
choice(input27, 18) -> {mc02_06,output};
choice(input27, 19) -> {mc04_16,output};
choice(input28,  1) -> {mc04_15,input};
choice(input28,  2) -> {mc04_08,input};
choice(input28,  3) -> {mc03_18,input};
choice(input28,  4) -> {mc01_15,input};
choice(input28, 16) -> {mc01_01,output};
choice(input28, 17) -> {mc03_17,output};
choice(input28, 18) -> {mc02_17,output};
choice(input28, 19) -> {mc04_15,output};
choice(input29,  1) -> {mc02_06,input};
choice(input29,  2) -> {mc04_04,input};
choice(input29,  3) -> {mc03_14,input};
choice(input29,  4) -> {mc03_07,input};
choice(input29, 16) -> {mc01_09,output};
choice(input29, 17) -> {mc03_09,output};
choice(input29, 18) -> {mc02_08,output};
choice(input29, 19) -> {mc04_13,output};
choice(input30,  1) -> {mc04_17,input};
choice(input30,  2) -> {mc04_03,input};
choice(input30,  3) -> {mc01_05,input};
choice(input30,  4) -> {mc01_10,input};
choice(input30, 16) -> {mc01_03,output};
choice(input30, 17) -> {mc03_07,output};
choice(input30, 18) -> {mc02_14,output};
choice(input30, 19) -> {mc04_14,output};
choice(input31,  1) -> {mc04_14,input};
choice(input31,  2) -> {mc01_02,input};
choice(input31,  3) -> {mc03_15,input};
choice(input31,  4) -> {mc03_01,input};
choice(input31, 16) -> {mc01_14,output};
choice(input31, 17) -> {mc03_01,output};
choice(input31, 18) -> {mc02_16,output};
choice(input31, 19) -> {mc04_06,output};
choice(input32,  1) -> {mc04_13,input};
choice(input32,  2) -> {mc04_05,input};
choice(input32,  3) -> {mc01_07,input};
choice(input32,  4) -> {mc01_14,input};
choice(input32, 16) -> {mc01_14,output};
choice(input32, 17) -> {mc03_08,output};
choice(input32, 18) -> {mc02_06,output};
choice(input32, 19) -> {mc04_04,output};
choice(input33,  1) -> {mc02_08,input};
choice(input33,  2) -> {mc04_07,input};
choice(input33,  3) -> {mc03_10,input};
choice(input33,  4) -> {mc01_17,input};
choice(input33, 16) -> {mc01_17,output};
choice(input33, 17) -> {mc03_10,output};
choice(input33, 18) -> {mc02_09,output};
choice(input33, 19) -> {mc04_08,output};
choice(input34,  1) -> {mc02_02,input};
choice(input34,  2) -> {mc02_12,input};
choice(input34,  3) -> {mc01_06,input};
choice(input34,  4) -> {mc03_06,input};
choice(input34, 16) -> {mc01_01,output};
choice(input34, 17) -> {mc03_18,output};
choice(input34, 18) -> {mc02_01,output};
choice(input34, 19) -> {mc04_09,output};
choice(input35,  1) -> {mc02_18,input};
choice(input35,  2) -> {mc04_02,input};
choice(input35,  3) -> {mc03_18,input};
choice(input35,  4) -> {mc03_03,input};
choice(input35, 16) -> {mc01_08,output};
choice(input35, 17) -> unknown;
choice(input35, 18) -> {mc02_04,output};
choice(input35, 19) -> {mc04_12,output};
choice(input36,  1) -> {mc04_12,input};
choice(input36,  2) -> {mc04_02,input};
choice(input36,  3) -> {mc03_12,input};
choice(input36,  4) -> {mc03_11,input};
choice(input36, 16) -> {mc01_02,output};
choice(input36, 17) -> {mc03_01,output};
choice(input36, 18) -> {mc02_15,output};
choice(input36, 19) -> {mc04_16,output};
choice(input37,  1) -> {mc02_01,input};
choice(input37,  2) -> {mc04_05,input};
choice(input37,  3) -> {mc03_17,input};
choice(input37,  4) -> {mc01_13,input};
choice(input37, 16) -> {mc01_16,output};
choice(input37, 17) -> {mc03_16,output};
choice(input37, 18) -> unknown;
choice(input37, 19) -> {mc04_09,output};
choice(input38,  1) -> {mc02_01,input};
choice(input38,  2) -> {mc02_16,input};
choice(input38,  3) -> {mc04_01,input};
choice(input38,  4) -> {mc03_04,input};
choice(input38, 16) -> {mc01_03,output};
choice(input38, 17) -> {mc03_11,output};
choice(input38, 18) -> {mc02_01,output};
choice(input38, 19) -> {mc04_01,output};
choice(input39,  1) -> {mc04_14,input};
choice(input39,  2) -> {mc04_11,input};
choice(input39,  3) -> {mc03_10,input};
choice(input39,  4) -> {mc01_16,input};
choice(input39, 16) -> {mc01_12,output};
choice(input39, 17) -> {mc03_09,output};
choice(input39, 18) -> {mc02_17,output};
choice(input39, 19) -> {mc04_10,output};
choice(input40,  1) -> {mc02_10,input};
choice(input40,  2) -> {mc04_06,input};
choice(input40,  3) -> {mc03_16,input};
choice(input40,  4) -> {mc01_12,input};
choice(input40, 16) -> {mc01_07,output};
choice(input40, 17) -> {mc03_04,output};
choice(input40, 18) -> {mc02_09,output};
choice(input40, 19) -> {mc04_07,output};
choice(input41,  1) -> {mc02_09,input};
choice(input41,  2) -> {mc01_02,input};
choice(input41,  3) -> {mc01_08,input};
choice(input41,  4) -> {mc03_04,input};
choice(input41, 16) -> {mc01_10,output};
choice(input41, 17) -> {mc03_03,output};
choice(input41, 18) -> unknown;
choice(input41, 19) -> {mc04_01,output};
choice(input42,  1) -> {mc04_18,input};
choice(input42,  2) -> {mc04_08,input};
choice(input42,  3) -> {mc01_07,input};
choice(input42,  4) -> {mc03_07,input};
choice(input42, 16) -> {mc01_09,output};
choice(input42, 17) -> {mc03_02,output};
choice(input42, 18) -> unknown;
choice(input42, 19) -> {mc04_11,output};
choice(input43,  1) -> {mc04_15,input};
choice(input43,  2) -> {mc02_14,input};
choice(input43,  3) -> {mc03_12,input};
choice(input43,  4) -> {mc03_02,input};
choice(input43, 16) -> unknown;
choice(input43, 17) -> unknown;
choice(input43, 18) -> {mc02_12,output};
choice(input43, 19) -> {mc04_11,output};
choice(input44,  1) -> {mc02_08,input};
choice(input44,  2) -> {mc04_04,input};
choice(input44,  3) -> {mc03_17,input};
choice(input44,  4) -> {mc01_18,input};
choice(input44, 16) -> {mc01_18,output};
choice(input44, 17) -> {mc03_10,output};
choice(input44, 18) -> {mc02_11,output};
choice(input44, 19) -> {mc04_03,output};
choice(input45,  1) -> {mc02_18,input};
choice(input45,  2) -> {mc02_13,input};
choice(input45,  3) -> {mc01_03,input};
choice(input45,  4) -> {mc01_17,input};
choice(input45, 16) -> {mc01_02,output};
choice(input45, 17) -> {mc03_17,output};
choice(input45, 18) -> {mc02_16,output};
choice(input45, 19) -> {mc04_12,output};
choice(input46,  1) -> {mc04_17,input};
choice(input46,  2) -> {mc02_15,input};
choice(input46,  3) -> {mc03_13,input};
choice(input46,  4) -> {mc01_16,input};
choice(input46, 16) -> {mc01_10,output};
choice(input46, 17) -> {mc03_03,output};
choice(input46, 18) -> {mc02_07,output};
choice(input46, 19) -> {mc04_17,output};
choice(input47,  1) -> {mc04_10,input};
choice(input47,  2) -> {mc04_09,input};
choice(input47,  3) -> {mc03_14,input};
choice(input47,  4) -> {mc03_05,input};
choice(input47, 16) -> {mc01_04,output};
choice(input47, 17) -> {mc03_06,output};
choice(input47, 18) -> {mc02_02,output};
choice(input47, 19) -> {mc04_17,output};
choice(input48,  1) -> {mc02_05,input};
choice(input48,  2) -> {mc02_17,input};
choice(input48,  3) -> {mc03_13,input};
choice(input48,  4) -> {mc03_11,input};
choice(input48, 16) -> {mc01_05,output};
choice(input48, 17) -> {mc03_02,output};
choice(input48, 18) -> {mc02_10,output};
choice(input48, 19) -> {mc04_18,output};
choice(input49,  1) -> {mc02_10,input};
choice(input49,  2) -> {mc02_12,input};
choice(input49,  3) -> {mc01_11,input};
choice(input49,  4) -> {mc01_13,input};
choice(input49, 16) -> {mc01_17,output};
choice(input49, 17) -> {mc03_14,output};
choice(input49, 18) -> {mc02_03,output};
choice(input49, 19) -> {mc04_04,output};
choice(input50,  1) -> {mc04_16,input};
choice(input50,  2) -> {mc02_11,input};
choice(input50,  3) -> {mc01_01,input};
choice(input50,  4) -> {mc01_10,input};
choice(input50, 16) -> {mc01_08,output};
choice(input50, 17) -> {mc03_11,output};
choice(input50, 18) -> {mc02_18,output};
choice(input50, 19) -> {mc04_07,output};
choice(input51,  1) -> {mc02_03,input};
choice(input51,  2) -> {mc02_11,input};
choice(input51,  3) -> {mc01_11,input};
choice(input51,  4) -> {mc03_06,input};
choice(input51, 16) -> {mc01_11,output};
choice(input51, 17) -> {mc03_12,output};
choice(input51, 18) -> {mc02_12,output};
choice(input51, 19) -> {mc04_10,output};
choice(input52,  1) -> {mc02_04,input};
choice(input52,  2) -> {mc04_07,input};
choice(input52,  3) -> {mc01_09,input};
choice(input52,  4) -> {mc03_09,input};
choice(input52, 16) -> {mc01_12,output};
choice(input52, 17) -> {mc03_15,output};
choice(input52, 18) -> {mc02_02,output};
choice(input52, 19) -> {mc04_13,output};
choice(input53,  1) -> {mc02_09,input};
choice(input53,  2) -> {mc04_03,input};
choice(input53,  3) -> {mc01_01,input};
choice(input53,  4) -> {mc01_12,input};
choice(input53, 16) -> {mc01_16,output};
choice(input53, 17) -> {mc03_18,output};
choice(input53, 18) -> {mc02_10,output};
choice(input53, 19) -> {mc04_02,output};
choice(input54,  1) -> {mc04_18,input};
choice(input54,  2) -> {mc02_07,input};
choice(input54,  3) -> {mc01_09,input};
choice(input54,  4) -> {mc01_15,input};
choice(input54, 16) -> {mc01_06,output};
choice(input54, 17) -> {mc03_07,output};
choice(input54, 18) -> {mc02_18,output};
choice(input54, 19) -> {mc04_18,output}.

%%====================================================================
%% sources
%%====================================================================

-spec sources(macro_cell(), direction())
    -> {choice(), [input()]} | no_pin | unknown.

sources(mc01_01, output) -> {16,[input01,input28,input34]};
sources(mc01_01, input) -> {3,[input05,input50,input53]};
sources(mc01_02, output) -> {16,[input02,input36,input45]};
sources(mc01_02, input) -> {2,[input17,input31,input41]};
sources(mc01_03, output) -> {16,[input03,input30,input38]};
sources(mc01_03, input) -> {3,[input09,input27,input45]};
sources(mc01_04, output) -> {16,[input04,input26,input47]};
sources(mc01_04, input) -> {3,[input11,input22,input26]};
sources(mc01_05, output) -> {16,[input05,input48]};
sources(mc01_05, input) -> {3,[input01,input21,input30]};
sources(mc01_06, output) -> {16,[input06,input22,input54]};
sources(mc01_06, input) -> {3,[input03,input23,input34]};
sources(mc01_07, output) -> {16,[input07,input25,input40]};
sources(mc01_07, input) -> {3,[input17,input32,input42]};
sources(mc01_08, output) -> {16,[input08,input35,input50]};
sources(mc01_08, input) -> {3,[input07,input20,input41]};
sources(mc01_09, output) -> {16,[input09,input29,input42]};
sources(mc01_09, input) -> {3,[input13,input52,input54]};
sources(mc01_10, output) -> {16,[input10,input41,input46]};
sources(mc01_10, input) -> {4,[input03,input30,input50]};
sources(mc01_11, output) -> {16,[input11,input21,input51]};
sources(mc01_11, input) -> {3,[input15,input49,input51]};
sources(mc01_12, output) -> {16,[input12,input39,input52]};
sources(mc01_12, input) -> {4,[input11,input40,input53]};
sources(mc01_13, output) -> {16,[input13]};
sources(mc01_13, input) -> {4,[input15,input37,input49]};
sources(mc01_14, output) -> {16,[input14,input31,input32]};
sources(mc01_14, input) -> {4,[input01,input25,input32]};
sources(mc01_15, output) -> {16,[input15,input20,input23]};
sources(mc01_15, input) -> {4,[input05,input28,input54]};
sources(mc01_16, output) -> {16,[input16,input37,input53]};
sources(mc01_16, input) -> {4,[input18,input39,input46]};
sources(mc01_17, output) -> {16,[input17,input33,input49]};
sources(mc01_17, input) -> {4,[input07,input33,input45]};
sources(mc01_18, output) -> {16,[input18,input24,input44]};
sources(mc01_18, input) -> {4,[input16,input24,input44]};
sources(mc02_01, output) -> {18,[input01,input34,input38]};
sources(mc02_01, input) -> {1,[input06,input37,input38]};
sources(mc02_02, output) -> {18,[input02,input47,input52]};
sources(mc02_02, input) -> {1,[input07,input20,input34]};
sources(mc02_03, output) -> {18,[input03,input26,input49]};
sources(mc02_03, input) -> {1,[input01,input25,input51]};
sources(mc02_04, output) -> {18,[input04,input19,input35]};
sources(mc02_04, input) -> {1,[input05,input19,input52]};
sources(mc02_05, output) -> {18,[input05,input21]};
sources(mc02_05, input) -> {1,[input09,input24,input48]};
sources(mc02_06, output) -> {18,[input06,input27,input32]};
sources(mc02_06, input) -> {1,[input11,input26,input29]};
sources(mc02_07, output) -> {18,[input07,input23,input46]};
sources(mc02_07, input) -> {2,[input01,input27,input54]};
sources(mc02_08, output) -> {18,[input08,input25,input29]};
sources(mc02_08, input) -> {1,[input13,input33,input44]};
sources(mc02_09, output) -> {18,[input09,input33,input40]};
sources(mc02_09, input) -> {1,[input15,input41,input53]};
sources(mc02_10, output) -> {18,[input10,input48,input53]};
sources(mc02_10, input) -> {1,[input17,input40,input49]};
sources(mc02_11, output) -> {18,[input11,input22,input44]};
sources(mc02_11, input) -> {2,[input03,input50,input51]};
sources(mc02_12, output) -> {18,[input12,input43,input51]};
sources(mc02_12, input) -> {2,[input05,input34,input49]};
sources(mc02_13, output) -> {18,[input13]};
sources(mc02_13, input) -> {2,[input07,input26,input45]};
sources(mc02_14, output) -> {18,[input14,input20,input30]};
sources(mc02_14, input) -> {2,[input09,input23,input43]};
sources(mc02_15, output) -> {18,[input15,input24,input36]};
sources(mc02_15, input) -> {2,[input13,input19,input46]};
sources(mc02_16, output) -> {18,[input16,input31,input45]};
sources(mc02_16, input) -> {2,[input11,input22,input38]};
sources(mc02_17, output) -> {18,[input17,input28,input39]};
sources(mc02_17, input) -> {2,[input15,input20,input48]};
sources(mc02_18, output) -> {18,[input18,input50,input54]};
sources(mc02_18, input) -> {1,[input03,input35,input45]};
sources(mc03_01, output) -> {17,[input01,input31,input36]};
sources(mc03_01, input) -> {4,[input14,input20,input31]};
sources(mc03_02, output) -> {17,[input02,input42,input48]};
sources(mc03_02, input) -> {4,[input09,input23,input43]};
sources(mc03_03, output) -> {17,[input03,input41,input46]};
sources(mc03_03, input) -> {4,[input10,input21,input35]};
sources(mc03_04, output) -> {17,[input04,input22,input40]};
sources(mc03_04, input) -> {4,[input08,input38,input41]};
sources(mc03_05, output) -> {17,[input05]};
sources(mc03_05, input) -> {4,[input13,input22,input47]};
sources(mc03_06, output) -> {17,[input06,input23,input47]};
sources(mc03_06, input) -> {4,[input04,input34,input51]};
sources(mc03_07, output) -> {17,[input07,input30,input54]};
sources(mc03_07, input) -> {4,[input02,input29,input42]};
sources(mc03_08, output) -> {17,[input08,input20,input32]};
sources(mc03_08, input) -> {4,[input17,input26,input27]};
sources(mc03_09, output) -> {17,[input09,input29,input39]};
sources(mc03_09, input) -> {4,[input12,input19,input52]};
sources(mc03_10, output) -> {17,[input10,input33,input44]};
sources(mc03_10, input) -> {3,[input10,input33,input39]};
sources(mc03_11, output) -> {17,[input11,input38,input50]};
sources(mc03_11, input) -> {4,[input06,input36,input48]};
sources(mc03_12, output) -> {17,[input12,input27,input51]};
sources(mc03_12, input) -> {3,[input08,input36,input43]};
sources(mc03_13, output) -> {17,[input13]};
sources(mc03_13, input) -> {3,[input06,input46,input48]};
sources(mc03_14, output) -> {17,[input14,input26,input49]};
sources(mc03_14, input) -> {3,[input18,input29,input47]};
sources(mc03_15, output) -> {17,[input15,input21,input52]};
sources(mc03_15, input) -> {3,[input16,input25,input31]};
sources(mc03_16, output) -> {17,[input16,input24,input37]};
sources(mc03_16, input) -> {3,[input04,input19,input40]};
sources(mc03_17, output) -> {17,[input17,input28,input45]};
sources(mc03_17, input) -> {3,[input14,input37,input44]};
sources(mc03_18, output) -> {17,[input18,input34,input53]};
sources(mc03_18, input) -> {3,[input12,input28,input35]};
sources(mc04_01, output) -> {19,[input01,input38,input41]};
sources(mc04_01, input) -> {3,[input02,input24,input38]};
sources(mc04_02, output) -> {19,[input02,input19,input53]};
sources(mc04_02, input) -> {2,[input16,input35,input36]};
sources(mc04_03, output) -> {19,[input03,input21,input44]};
sources(mc04_03, input) -> {2,[input10,input30,input53]};
sources(mc04_04, output) -> {19,[input04,input32,input49]};
sources(mc04_04, input) -> {2,[input08,input29,input44]};
sources(mc04_05, output) -> {19,[input05]};
sources(mc04_05, input) -> {2,[input14,input32,input37]};
sources(mc04_06, output) -> {19,[input06,input20,input31]};
sources(mc04_06, input) -> {2,[input04,input21,input40]};
sources(mc04_07, output) -> {19,[input07,input40,input50]};
sources(mc04_07, input) -> {2,[input02,input33,input52]};
sources(mc04_08, output) -> {19,[input08,input25,input33]};
sources(mc04_08, input) -> {2,[input12,input28,input42]};
sources(mc04_09, output) -> {19,[input09,input34,input37]};
sources(mc04_09, input) -> {2,[input18,input25,input47]};
sources(mc04_10, output) -> {19,[input10,input39,input51]};
sources(mc04_10, input) -> {1,[input14,input21,input47]};
sources(mc04_11, output) -> {19,[input11,input42,input43]};
sources(mc04_11, input) -> {2,[input06,input24,input39]};
sources(mc04_12, output) -> {19,[input12,input35,input45]};
sources(mc04_12, input) -> {1,[input12,input22,input36]};
sources(mc04_13, output) -> {19,[input13,input29,input52]};
sources(mc04_13, input) -> {1,[input10,input27,input32]};
sources(mc04_14, output) -> {19,[input14,input26,input30]};
sources(mc04_14, input) -> {1,[input18,input31,input39]};
sources(mc04_15, output) -> {19,[input15,input24,input28]};
sources(mc04_15, input) -> {1,[input04,input28,input43]};
sources(mc04_16, output) -> {19,[input16,input27,input36]};
sources(mc04_16, input) -> {1,[input08,input23,input50]};
sources(mc04_17, output) -> {19,[input17,input46,input47]};
sources(mc04_17, input) -> {1,[input02,input30,input46]};
sources(mc04_18, output) -> {19,[input18,input48,input54]};
sources(mc04_18, input) -> {1,[input16,input42,input54]}.

