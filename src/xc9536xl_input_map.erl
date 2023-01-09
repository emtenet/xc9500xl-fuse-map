-module(xc9536xl_input_map).

-export([choices/0]).
-export([choice/2]).
-export([sources/2]).

-type input() :: input:input().
-type macro_cell() :: macro_cell:absolute().
-type choice() :: input:choice().
-type realm() :: input:realm().
-type source() :: input:source().

%%====================================================================
%% choices
%%====================================================================

-spec choices() -> [choice()].

choices() ->
    [1,2,16,17].

%%====================================================================
%% choice
%%====================================================================

-spec choice(input(), choice()) -> source() | unknown.

choice(input01,  1) -> {mc02_09, external};
choice(input01,  2) -> {mc01_01, external};
choice(input01, 16) -> {mc01_01, internal};
choice(input01, 17) -> {mc02_01, internal};
choice(input02,  1) -> {mc02_10, external};
choice(input02,  2) -> {mc01_17, external};
choice(input02, 16) -> {mc01_02, internal};
choice(input02, 17) -> {mc02_02, internal};
choice(input03,  1) -> {mc02_08, external};
choice(input03,  2) -> {mc01_02, external};
choice(input03, 16) -> {mc01_03, internal};
choice(input03, 17) -> {mc02_03, internal};
choice(input04,  1) -> {mc02_11, external};
choice(input04,  2) -> {mc01_18, external};
choice(input04, 16) -> {mc01_04, internal};
choice(input04, 17) -> {mc02_04, internal};
choice(input05,  1) -> {mc02_07, external};
choice(input05,  2) -> {mc01_04, external};
choice(input05, 16) -> {mc01_05, internal};
choice(input05, 17) -> {mc02_05, internal};
choice(input06,  1) -> {mc02_12, external};
choice(input06,  2) -> {mc01_16, external};
choice(input06, 16) -> {mc01_06, internal};
choice(input06, 17) -> {mc02_06, internal};
choice(input07,  1) -> {mc02_06, external};
choice(input07,  2) -> {mc01_03, external};
choice(input07, 16) -> {mc01_07, internal};
choice(input07, 17) -> {mc02_07, internal};
choice(input08,  1) -> {mc02_13, external};
choice(input08,  2) -> {mc01_15, external};
choice(input08, 16) -> {mc01_08, internal};
choice(input08, 17) -> {mc02_08, internal};
choice(input09,  1) -> {mc02_05, external};
choice(input09,  2) -> {mc01_05, external};
choice(input09, 16) -> {mc01_09, internal};
choice(input09, 17) -> {mc02_09, internal};
choice(input10,  1) -> {mc02_18, external};
choice(input10,  2) -> {mc01_14, external};
choice(input10, 16) -> {mc01_10, internal};
choice(input10, 17) -> {mc02_10, internal};
choice(input11,  1) -> {mc02_03, external};
choice(input11,  2) -> {mc01_07, external};
choice(input11, 16) -> {mc01_11, internal};
choice(input11, 17) -> {mc02_11, internal};
choice(input12,  1) -> {mc02_14, external};
choice(input12,  2) -> {mc01_13, external};
choice(input12, 16) -> {mc01_12, internal};
choice(input12, 17) -> {mc02_12, internal};
choice(input13,  1) -> {mc02_04, external};
choice(input13,  2) -> {mc01_06, external};
choice(input13, 16) -> {mc01_13, internal};
choice(input13, 17) -> {mc02_13, internal};
choice(input14,  1) -> {mc02_15, external};
choice(input14,  2) -> {mc01_12, external};
choice(input14, 16) -> {mc01_14, internal};
choice(input14, 17) -> {mc02_14, internal};
choice(input15,  1) -> {mc02_02, external};
choice(input15,  2) -> {mc01_08, external};
choice(input15, 16) -> {mc01_15, internal};
choice(input15, 17) -> {mc02_15, internal};
choice(input16,  1) -> {mc02_16, external};
choice(input16,  2) -> {mc01_11, external};
choice(input16, 16) -> {mc01_16, internal};
choice(input16, 17) -> {mc02_16, internal};
choice(input17,  1) -> {mc02_01, external};
choice(input17,  2) -> {mc01_09, external};
choice(input17, 16) -> {mc01_17, internal};
choice(input17, 17) -> {mc02_17, internal};
choice(input18,  1) -> {mc02_17, external};
choice(input18,  2) -> {mc01_10, external};
choice(input18, 16) -> {mc01_18, internal};
choice(input18, 17) -> {mc02_18, internal};
choice(input19,  1) -> {mc02_07, external};
choice(input19,  2) -> {mc01_06, external};
choice(input19, 16) -> {mc01_04, internal};
choice(input19, 17) -> {mc02_12, internal};
choice(input20,  1) -> {mc02_06, external};
choice(input20,  2) -> {mc01_08, external};
choice(input20, 16) -> unknown;
choice(input20, 17) -> {mc02_14, internal};
choice(input21,  1) -> {mc02_15, external};
choice(input21,  2) -> {mc01_18, external};
choice(input21, 16) -> {mc01_01, internal};
choice(input21, 17) -> {mc02_10, internal};
choice(input22,  1) -> {mc02_14, external};
choice(input22,  2) -> {mc01_07, external};
choice(input22, 16) -> {mc01_11, internal};
choice(input22, 17) -> {mc02_13, internal};
choice(input23,  1) -> {mc02_13, external};
choice(input23,  2) -> {mc01_05, external};
choice(input23, 16) -> {mc01_03, internal};
choice(input23, 17) -> {mc02_09, internal};
choice(input24,  1) -> {mc02_05, external};
choice(input24,  2) -> {mc01_16, external};
choice(input24, 16) -> {mc01_02, internal};
choice(input24, 17) -> {mc02_16, internal};
choice(input25,  1) -> {mc02_09, external};
choice(input25,  2) -> {mc01_10, external};
choice(input25, 16) -> {mc01_16, internal};
choice(input25, 17) -> {mc02_01, internal};
choice(input26,  1) -> {mc02_03, external};
choice(input26,  2) -> {mc01_03, external};
choice(input26, 16) -> {mc01_11, internal};
choice(input26, 17) -> {mc02_17, internal};
choice(input27,  1) -> {mc02_18, external};
choice(input27,  2) -> {mc01_01, external};
choice(input27, 16) -> {mc01_09, internal};
choice(input27, 17) -> {mc02_17, internal};
choice(input28,  1) -> {mc02_11, external};
choice(input28,  2) -> {mc01_13, external};
choice(input28, 16) -> {mc01_12, internal};
choice(input28, 17) -> {mc02_05, internal};
choice(input29,  1) -> {mc02_03, external};
choice(input29,  2) -> {mc01_15, external};
choice(input29, 16) -> {mc01_18, internal};
choice(input29, 17) -> {mc02_02, internal};
choice(input30,  1) -> {mc02_10, external};
choice(input30,  2) -> {mc01_14, external};
choice(input30, 16) -> {mc01_01, internal};
choice(input30, 17) -> {mc02_03, internal};
choice(input31,  1) -> {mc02_17, external};
choice(input31,  2) -> {mc01_09, external};
choice(input31, 16) -> {mc01_16, internal};
choice(input31, 17) -> {mc02_14, internal};
choice(input32,  1) -> {mc02_18, external};
choice(input32,  2) -> {mc01_12, external};
choice(input32, 16) -> {mc01_17, internal};
choice(input32, 17) -> unknown;
choice(input33,  1) -> {mc02_04, external};
choice(input33,  2) -> {mc01_17, external};
choice(input33, 16) -> {mc01_10, internal};
choice(input33, 17) -> unknown;
choice(input34,  1) -> {mc02_06, external};
choice(input34,  2) -> {mc01_04, external};
choice(input34, 16) -> unknown;
choice(input34, 17) -> {mc02_04, internal};
choice(input35,  1) -> {mc02_08, external};
choice(input35,  2) -> {mc01_11, external};
choice(input35, 16) -> {mc01_12, internal};
choice(input35, 17) -> {mc02_10, internal};
choice(input36,  1) -> {mc02_14, external};
choice(input36,  2) -> {mc01_11, external};
choice(input36, 16) -> {mc01_08, internal};
choice(input36, 17) -> {mc02_06, internal};
choice(input37,  1) -> {mc02_12, external};
choice(input37,  2) -> {mc01_12, external};
choice(input37, 16) -> {mc01_14, internal};
choice(input37, 17) -> {mc02_15, internal};
choice(input38,  1) -> {mc02_12, external};
choice(input38,  2) -> {mc01_07, external};
choice(input38, 16) -> {mc01_02, internal};
choice(input38, 17) -> {mc02_08, internal};
choice(input39,  1) -> {mc02_17, external};
choice(input39,  2) -> {mc01_16, external};
choice(input39, 16) -> {mc01_10, internal};
choice(input39, 17) -> {mc02_18, internal};
choice(input40,  1) -> {mc02_01, external};
choice(input40,  2) -> {mc01_18, external};
choice(input40, 16) -> {mc01_04, internal};
choice(input40, 17) -> {mc02_11, internal};
choice(input41,  1) -> {mc02_02, external};
choice(input41,  2) -> {mc01_09, external};
choice(input41, 16) -> unknown;
choice(input41, 17) -> {mc02_08, internal};
choice(input42,  1) -> {mc02_16, external};
choice(input42,  2) -> {mc01_13, external};
choice(input42, 16) -> {mc01_17, internal};
choice(input42, 17) -> {mc02_02, internal};
choice(input43,  1) -> {mc02_11, external};
choice(input43,  2) -> {mc01_05, external};
choice(input43, 16) -> {mc01_08, internal};
choice(input43, 17) -> {mc02_09, internal};
choice(input44,  1) -> {mc02_04, external};
choice(input44,  2) -> {mc01_15, external};
choice(input44, 16) -> {mc01_14, internal};
choice(input44, 17) -> {mc02_16, internal};
choice(input45,  1) -> {mc02_08, external};
choice(input45,  2) -> {mc01_03, external};
choice(input45, 16) -> {mc01_09, internal};
choice(input45, 17) -> {mc02_07, internal};
choice(input46,  1) -> {mc02_10, external};
choice(input46,  2) -> {mc01_06, external};
choice(input46, 16) -> {mc01_06, internal};
choice(input46, 17) -> {mc02_18, internal};
choice(input47,  1) -> {mc02_15, external};
choice(input47,  2) -> {mc01_10, external};
choice(input47, 16) -> {mc01_18, internal};
choice(input47, 17) -> {mc02_13, internal};
choice(input48,  1) -> {mc02_05, external};
choice(input48,  2) -> {mc01_08, external};
choice(input48, 16) -> {mc01_06, internal};
choice(input48, 17) -> {mc02_06, internal};
choice(input49,  1) -> {mc02_01, external};
choice(input49,  2) -> {mc01_04, external};
choice(input49, 16) -> {mc01_15, internal};
choice(input49, 17) -> {mc02_15, internal};
choice(input50,  1) -> {mc02_13, external};
choice(input50,  2) -> {mc01_02, external};
choice(input50, 16) -> {mc01_05, internal};
choice(input50, 17) -> {mc02_03, internal};
choice(input51,  1) -> {mc02_09, external};
choice(input51,  2) -> {mc01_02, external};
choice(input51, 16) -> {mc01_15, internal};
choice(input51, 17) -> {mc02_04, internal};
choice(input52,  1) -> {mc02_07, external};
choice(input52,  2) -> {mc01_17, external};
choice(input52, 16) -> {mc01_13, internal};
choice(input52, 17) -> {mc02_12, internal};
choice(input53,  1) -> {mc02_02, external};
choice(input53,  2) -> {mc01_14, external};
choice(input53, 16) -> {mc01_05, internal};
choice(input53, 17) -> {mc02_11, internal};
choice(input54,  1) -> {mc02_16, external};
choice(input54,  2) -> {mc01_01, external};
choice(input54, 16) -> {mc01_13, internal};
choice(input54, 17) -> {mc02_05, internal}.

%%====================================================================
%% sources
%%====================================================================

-spec sources(macro_cell(), realm())
    -> {choice(), [input()]} | no_pin | unknown.

sources(mc01_01, internal) -> {16, [input01, input21, input30]};
sources(mc01_01, external) -> { 2, [input01, input27, input54]};
sources(mc01_02, internal) -> {16, [input02, input24, input38]};
sources(mc01_02, external) -> { 2, [input03, input50, input51]};
sources(mc01_03, internal) -> {16, [input03, input23]};
sources(mc01_03, external) -> { 2, [input07, input26, input45]};
sources(mc01_04, internal) -> {16, [input04, input19, input40]};
sources(mc01_04, external) -> { 2, [input05, input34, input49]};
sources(mc01_05, internal) -> {16, [input05, input50, input53]};
sources(mc01_05, external) -> { 2, [input09, input23, input43]};
sources(mc01_06, internal) -> {16, [input06, input46, input48]};
sources(mc01_06, external) -> { 2, [input13, input19, input46]};
sources(mc01_07, internal) -> {16, [input07]};
sources(mc01_07, external) -> { 2, [input11, input22, input38]};
sources(mc01_08, internal) -> {16, [input08, input36, input43]};
sources(mc01_08, external) -> { 2, [input15, input20, input48]};
sources(mc01_09, internal) -> {16, [input09, input27, input45]};
sources(mc01_09, external) -> { 2, [input17, input31, input41]};
sources(mc01_10, internal) -> {16, [input10, input33, input39]};
sources(mc01_10, external) -> { 2, [input18, input25, input47]};
sources(mc01_11, internal) -> {16, [input11, input22, input26]};
sources(mc01_11, external) -> { 2, [input16, input35, input36]};
sources(mc01_12, internal) -> {16, [input12, input28, input35]};
sources(mc01_12, external) -> { 2, [input14, input32, input37]};
sources(mc01_13, internal) -> {16, [input13, input52, input54]};
sources(mc01_13, external) -> { 2, [input12, input28, input42]};
sources(mc01_14, internal) -> {16, [input14, input37, input44]};
sources(mc01_14, external) -> { 2, [input10, input30, input53]};
sources(mc01_15, internal) -> {16, [input15, input49, input51]};
sources(mc01_15, external) -> { 2, [input08, input29, input44]};
sources(mc01_16, internal) -> {16, [input16, input25, input31]};
sources(mc01_16, external) -> { 2, [input06, input24, input39]};
sources(mc01_17, internal) -> {16, [input17, input32, input42]};
sources(mc01_17, external) -> { 2, [input02, input33, input52]};
sources(mc01_18, internal) -> {16, [input18, input29, input47]};
sources(mc01_18, external) -> { 2, [input04, input21, input40]};
sources(mc02_01, internal) -> {17, [input01, input25]};
sources(mc02_01, external) -> { 1, [input17, input40, input49]};
sources(mc02_02, internal) -> {17, [input02, input29, input42]};
sources(mc02_02, external) -> { 1, [input15, input41, input53]};
sources(mc02_03, internal) -> {17, [input03, input30, input50]};
sources(mc02_03, external) -> { 1, [input11, input26, input29]};
sources(mc02_04, internal) -> {17, [input04, input34, input51]};
sources(mc02_04, external) -> { 1, [input13, input33, input44]};
sources(mc02_05, internal) -> {17, [input05, input28, input54]};
sources(mc02_05, external) -> { 1, [input09, input24, input48]};
sources(mc02_06, internal) -> {17, [input06, input36, input48]};
sources(mc02_06, external) -> { 1, [input07, input20, input34]};
sources(mc02_07, internal) -> {17, [input07, input45]};
sources(mc02_07, external) -> { 1, [input05, input19, input52]};
sources(mc02_08, internal) -> {17, [input08, input38, input41]};
sources(mc02_08, external) -> { 1, [input03, input35, input45]};
sources(mc02_09, internal) -> {17, [input09, input23, input43]};
sources(mc02_09, external) -> { 1, [input01, input25, input51]};
sources(mc02_10, internal) -> {17, [input10, input21, input35]};
sources(mc02_10, external) -> { 1, [input02, input30, input46]};
sources(mc02_11, internal) -> {17, [input11, input40, input53]};
sources(mc02_11, external) -> { 1, [input04, input28, input43]};
sources(mc02_12, internal) -> {17, [input12, input19, input52]};
sources(mc02_12, external) -> { 1, [input06, input37, input38]};
sources(mc02_13, internal) -> {17, [input13, input22, input47]};
sources(mc02_13, external) -> { 1, [input08, input23, input50]};
sources(mc02_14, internal) -> {17, [input14, input20, input31]};
sources(mc02_14, external) -> { 1, [input12, input22, input36]};
sources(mc02_15, internal) -> {17, [input15, input37, input49]};
sources(mc02_15, external) -> { 1, [input14, input21, input47]};
sources(mc02_16, internal) -> {17, [input16, input24, input44]};
sources(mc02_16, external) -> { 1, [input16, input42, input54]};
sources(mc02_17, internal) -> {17, [input17, input26, input27]};
sources(mc02_17, external) -> { 1, [input18, input31, input39]};
sources(mc02_18, internal) -> {17, [input18, input39, input46]};
sources(mc02_18, external) -> { 1, [input10, input27, input32]}.

