-module(input_map_physical).

-export([results/1]).

% The following input mapping results where obtained by programming
% a physical device with a test fixture and then using JTAG INTEST mode
% to collect the results.
%
% Once such ficture was for choice 16 of xc9572xl.
%
% Experiments had found:
%
%   sources(mc01_01, internal) -> {16, [input01, input28, input34]};
%   sources(mc01_02, internal) -> {16, [input02, input36, input45]};
%   sources(mc01_03, internal) -> {16, [input03, input30, input38]};
%   sources(mc01_04, internal) -> {16, [input04, input26, input47]};
%   sources(mc01_05, internal) -> {16, [input05, input48]};
%   sources(mc01_06, internal) -> {16, [input06, input22, input54]};
%   sources(mc01_07, internal) -> {16, [input07, input25, input40]};
%   sources(mc01_08, internal) -> {16, [input08, input35, input50]};
%   sources(mc01_09, internal) -> {16, [input09, input29, input42]};
%   sources(mc01_10, internal) -> {16, [input10, input41, input46]};
%   sources(mc01_11, internal) -> {16, [input11, input21, input51]};
%   sources(mc01_12, internal) -> {16, [input12, input39, input52]};
%   sources(mc01_13, internal) -> {16, [input13]};
%   sources(mc01_14, internal) -> {16, [input14, input31, input32]};
%   sources(mc01_15, internal) -> {16, [input15, input20, input23]};
%   sources(mc01_16, internal) -> {16, [input16, input37, input53]};
%   sources(mc01_17, internal) -> {16, [input17, input33, input49]};
%   sources(mc01_18, internal) -> {16, [input18, input24, input44]};
%
% where three inputs where unknown:
%  - input19, input27 & input43
% across two macro cells:
%  - mc01_05 & mc01_13.
%
% The following fixture was programmed:
%
%   driven inputs:
%       mc02_01
%       mc02_02
%
%   internal macro cells:
%       mc01_05: bypass = mc02_01.external
%       mc01_13: bypass = mc02_02.external
%
%   sensed outputs:
%       mc03_01: bypass = input05, oe = true
%       mc03_02: bypass = input13, oe = true
%       mc03_03: bypass = input19, oe = true
%       mc03_04: bypass = input27, oe = true
%       mc03_05: bypass = input43, oe = true
%       mc03_06: bypass = input48, oe = true
%
% In JTAG INTEST mode:
%
%   driving mc02_01 = 1, mc02_02 = 0 (internal mc01_05)
%       sensed mc03_01 = 1 (input05)
%       sensed mc03_02 = 0 (input13)
%       sensed mc03_03 = 1 (input19)
%       sensed mc03_04 = 0 (input27)
%       sensed mc03_05 = 0 (input43)
%       sensed mc03_06 = 1 (input48)
%
%   driving mc02_01 = 0, mc02_02 = 1 (internal mc01_13)
%       sensed mc03_01 = 0 (input05)
%       sensed mc03_02 = 1 (input13)
%       sensed mc03_03 = 0 (input19)
%       sensed mc03_04 = 1 (input27)
%       sensed mc03_05 = 1 (input43)
%       sensed mc03_06 = 0 (input48)
%
% Resulting in:
%
%   sources(mc01_05, internal) -> {16, [input05, input19, input48]};
%   sources(mc01_13, internal) -> {16, [input13, input27, input43]};

%%====================================================================
%% results
%%====================================================================

results(xc9572xl) ->
    [{fb01, input05, 16, {mc01_05, internal}},
     {fb01, input19, 16, {mc01_05, internal}},
     {fb01, input48, 16, {mc01_05, internal}},
     {fb01, input13, 16, {mc01_13, internal}},
     {fb01, input27, 16, {mc01_13, internal}},
     {fb01, input43, 16, {mc01_13, internal}},
     {fb01, input05, 17, {mc03_05, internal}},
     {fb01, input19, 17, {mc03_05, internal}},
     {fb01, input25, 17, {mc03_05, internal}},
     {fb01, input13, 17, {mc03_13, internal}},
     {fb01, input35, 17, {mc03_13, internal}},
     {fb01, input43, 17, {mc03_13, internal}},
     {fb01, input05, 18, {mc02_05, internal}},
     {fb01, input21, 18, {mc02_05, internal}},
     {fb01, input41, 18, {mc02_05, internal}},
     {fb01, input13, 18, {mc02_13, internal}},
     {fb01, input37, 18, {mc02_13, internal}},
     {fb01, input42, 18, {mc02_13, internal}},
     {fb01, input05, 19, {mc04_05, internal}},
     {fb01, input22, 19, {mc04_05, internal}},
     {fb01, input23, 19, {mc04_05, internal}},
     {fb01, input13, 19, {mc04_13, internal}},
     {fb01, input29, 19, {mc04_13, internal}},
     {fb01, input52, 19, {mc04_13, internal}}
    ];
results(xc95288xl) ->
    [{fb01, input47,  3, {mc04_03, external}},
     {fb01, input54,  9, {mc11_17, external}},
     {fb01, input50, 11, {mc07_17, external}},
     {fb01, input11, 16, {mc01_11, internal}},
     {fb01, input12, 16, {mc01_12, internal}},
     {fb01, input14, 16, {mc01_14, internal}},
     {fb01, input18, 16, {mc01_18, internal}},
     {fb01, input22, 16, {mc01_12, internal}},
     {fb01, input27, 16, {mc01_11, internal}},
     {fb01, input29, 16, {mc01_18, internal}},
     {fb01, input35, 16, {mc01_11, internal}},
     {fb01, input41, 16, {mc01_18, internal}},
     {fb01, input43, 16, {mc01_14, internal}},
     {fb01, input47, 16, {mc01_14, internal}},
     {fb01, input54, 16, {mc01_12, internal}},
     {fb01, input03, 17, {mc03_03, internal}},
     {fb01, input06, 17, {mc03_06, internal}},
     {fb01, input07, 17, {mc03_07, internal}},
     {fb01, input11, 17, {mc03_11, internal}},
     {fb01, input12, 17, {mc03_12, internal}},
     {fb01, input13, 17, {mc03_13, internal}},
     {fb01, input19, 17, {mc03_03, internal}},
     {fb01, input20, 17, {mc03_11, internal}},
     {fb01, input23, 17, {mc03_12, internal}},
     {fb01, input27, 17, {mc03_06, internal}},
     {fb01, input29, 17, {mc03_11, internal}},
     {fb01, input37, 17, {mc03_07, internal}},
     {fb01, input43, 17, {mc03_07, internal}},
     {fb01, input44, 17, {mc03_12, internal}},
     {fb01, input46, 17, {mc03_13, internal}},
     {fb01, input48, 17, {mc03_13, internal}},
     {fb01, input49, 17, {mc03_06, internal}},
     {fb01, input52, 17, {mc03_03, internal}},
     {fb01, input04, 18, {mc05_04, internal}},
     {fb01, input12, 18, {mc05_12, internal}},
     {fb01, input13, 18, {mc05_13, internal}},
     {fb01, input17, 18, {mc05_17, internal}},
     {fb01, input18, 18, {mc05_18, internal}},
     {fb01, input19, 18, {mc05_04, internal}},
     {fb01, input27, 18, {mc05_18, internal}},
     {fb01, input29, 18, {mc05_17, internal}},
     {fb01, input30, 18, {mc05_12, internal}},
     {fb01, input38, 18, {mc05_17, internal}},
     {fb01, input43, 18, {mc05_04, internal}},
     {fb01, input47, 18, {mc05_13, internal}},
     {fb01, input48, 18, {mc05_18, internal}},
     {fb01, input50, 18, {mc05_13, internal}},
     {fb01, input54, 18, {mc05_12, internal}},
     {fb01, input03, 19, {mc07_03, internal}},
     {fb01, input05, 19, {mc07_05, internal}},
     {fb01, input06, 19, {mc07_06, internal}},
     {fb01, input08, 19, {mc07_08, internal}},
     {fb01, input22, 19, {mc07_05, internal}},
     {fb01, input27, 19, {mc07_06, internal}},
     {fb01, input29, 19, {mc07_05, internal}},
     {fb01, input31, 19, {mc07_03, internal}},
     {fb01, input35, 19, {mc07_08, internal}},
     {fb01, input40, 19, {mc07_06, internal}},
     {fb01, input43, 19, {mc07_08, internal}},
     {fb01, input52, 19, {mc07_03, internal}},
     {fb01, input05, 20, {mc09_05, internal}},
     {fb01, input06, 20, {mc09_06, internal}},
     {fb01, input15, 20, {mc09_15, internal}},
     {fb01, input23, 20, {mc09_06, internal}},
     {fb01, input27, 20, {mc09_05, internal}},
     {fb01, input29, 20, {mc09_06, internal}},
     {fb01, input39, 20, {mc09_05, internal}},
     {fb01, input40, 20, {mc09_15, internal}},
     {fb01, input43, 20, {mc09_15, internal}},
     {fb01, input03, 21, {mc11_03, internal}},
     {fb01, input05, 21, {mc11_05, internal}},
     {fb01, input12, 21, {mc11_12, internal}},
     {fb01, input24, 21, {mc11_12, internal}},
     {fb01, input29, 21, {mc11_05, internal}},
     {fb01, input35, 21, {mc11_12, internal}},
     {fb01, input43, 21, {mc11_03, internal}},
     {fb01, input49, 21, {mc11_03, internal}},
     {fb01, input51, 21, {mc11_05, internal}},
     {fb01, input12, 22, {mc13_12, internal}},
     {fb01, input15, 22, {mc13_15, internal}},
     {fb01, input17, 22, {mc13_17, internal}},
     {fb01, input20, 22, {mc13_17, internal}},
     {fb01, input27, 22, {mc13_17, internal}},
     {fb01, input29, 22, {mc13_15, internal}},
     {fb01, input42, 22, {mc13_12, internal}},
     {fb01, input46, 22, {mc13_12, internal}},
     {fb01, input49, 22, {mc13_15, internal}},
     {fb01, input01, 23, {mc15_01, internal}},
     {fb01, input04, 23, {mc15_04, internal}},
     {fb01, input07, 23, {mc15_07, internal}},
     {fb01, input09, 23, {mc15_09, internal}},
     {fb01, input12, 23, {mc15_12, internal}},
     {fb01, input27, 23, {mc15_01, internal}},
     {fb01, input29, 23, {mc15_07, internal}},
     {fb01, input30, 23, {mc15_12, internal}},
     {fb01, input32, 23, {mc15_07, internal}},
     {fb01, input34, 23, {mc15_12, internal}},
     {fb01, input38, 23, {mc15_01, internal}},
     {fb01, input39, 23, {mc15_04, internal}},
     {fb01, input43, 23, {mc15_09, internal}},
     {fb01, input49, 23, {mc15_09, internal}},
     {fb01, input50, 23, {mc15_04, internal}},
     {fb01, input01, 24, {mc02_01, internal}},
     {fb01, input03, 24, {mc02_03, internal}},
     {fb01, input06, 24, {mc02_06, internal}},
     {fb01, input12, 24, {mc02_12, internal}},
     {fb01, input13, 24, {mc02_13, internal}},
     {fb01, input25, 24, {mc02_03, internal}},
     {fb01, input27, 24, {mc02_03, internal}},
     {fb01, input29, 24, {mc02_01, internal}},
     {fb01, input32, 24, {mc02_12, internal}},
     {fb01, input41, 24, {mc02_01, internal}},
     {fb01, input43, 24, {mc02_06, internal}},
     {fb01, input46, 24, {mc02_06, internal}},
     {fb01, input47, 24, {mc02_13, internal}},
     {fb01, input50, 24, {mc02_13, internal}},
     {fb01, input52, 24, {mc02_12, internal}},
     {fb01, input05, 25, {mc04_05, internal}},
     {fb01, input06, 25, {mc04_06, internal}},
     {fb01, input07, 25, {mc04_07, internal}},
     {fb01, input12, 25, {mc04_12, internal}},
     {fb01, input18, 25, {mc04_18, internal}},
     {fb01, input24, 25, {mc04_12, internal}},
     {fb01, input27, 25, {mc04_06, internal}},
     {fb01, input28, 25, {mc04_18, internal}},
     {fb01, input29, 25, {mc04_18, internal}},
     {fb01, input33, 25, {mc04_05, internal}},
     {fb01, input37, 25, {mc04_12, internal}},
     {fb01, input43, 25, {mc04_05, internal}},
     {fb01, input50, 25, {mc04_07, internal}},
     {fb01, input51, 25, {mc04_06, internal}},
     {fb01, input52, 25, {mc04_07, internal}},
     {fb01, input09, 26, {mc06_09, internal}},
     {fb01, input11, 26, {mc06_11, internal}},
     {fb01, input13, 26, {mc06_13, internal}},
     {fb01, input14, 26, {mc06_14, internal}},
     {fb01, input21, 26, {mc06_14, internal}},
     {fb01, input25, 26, {mc06_13, internal}},
     {fb01, input27, 26, {mc06_14, internal}},
     {fb01, input29, 26, {mc06_11, internal}},
     {fb01, input34, 26, {mc06_11, internal}},
     {fb01, input38, 26, {mc06_09, internal}},
     {fb01, input43, 26, {mc06_09, internal}},
     {fb01, input47, 26, {mc06_13, internal}},
     {fb01, input08, 27, {mc08_08, internal}},
     {fb01, input09, 27, {mc08_09, internal}},
     {fb01, input11, 27, {mc08_11, internal}},
     {fb01, input12, 27, {mc08_12, internal}},
     {fb01, input17, 27, {mc08_17, internal}},
     {fb01, input19, 27, {mc08_12, internal}},
     {fb01, input20, 27, {mc08_17, internal}},
     {fb01, input27, 27, {mc08_11, internal}},
     {fb01, input29, 27, {mc08_08, internal}},
     {fb01, input43, 27, {mc08_09, internal}},
     {fb01, input48, 27, {mc08_09, internal}},
     {fb01, input50, 27, {mc08_12, internal}},
     {fb01, input52, 27, {mc08_17, internal}},
     {fb01, input53, 27, {mc08_08, internal}},
     {fb01, input54, 27, {mc08_11, internal}},
     {fb01, input03, 28, {mc10_03, internal}},
     {fb01, input04, 28, {mc10_04, internal}},
     {fb01, input16, 28, {mc10_16, internal}},
     {fb01, input18, 28, {mc10_18, internal}},
     {fb01, input21, 28, {mc10_03, internal}},
     {fb01, input23, 28, {mc10_16, internal}},
     {fb01, input27, 28, {mc10_18, internal}},
     {fb01, input29, 28, {mc10_16, internal}},
     {fb01, input34, 28, {mc10_04, internal}},
     {fb01, input35, 28, {mc10_18, internal}},
     {fb01, input43, 28, {mc10_04, internal}},
     {fb01, input52, 28, {mc10_03, internal}},
     {fb01, input08, 29, {mc12_08, internal}},
     {fb01, input10, 29, {mc12_10, internal}},
     {fb01, input12, 29, {mc12_12, internal}},
     {fb01, input16, 29, {mc12_16, internal}},
     {fb01, input23, 29, {mc12_12, internal}},
     {fb01, input25, 29, {mc12_08, internal}},
     {fb01, input27, 29, {mc12_16, internal}},
     {fb01, input29, 29, {mc12_08, internal}},
     {fb01, input34, 29, {mc12_10, internal}},
     {fb01, input42, 29, {mc12_16, internal}},
     {fb01, input43, 29, {mc12_10, internal}},
     {fb01, input49, 29, {mc12_12, internal}},
     {fb01, input12, 30, {mc14_12, internal}},
     {fb01, input13, 30, {mc14_13, internal}},
     {fb01, input16, 30, {mc14_16, internal}},
     {fb01, input17, 30, {mc14_17, internal}},
     {fb01, input24, 30, {mc14_16, internal}},
     {fb01, input27, 30, {mc14_13, internal}},
     {fb01, input29, 30, {mc14_17, internal}},
     {fb01, input32, 30, {mc14_17, internal}},
     {fb01, input43, 30, {mc14_16, internal}},
     {fb01, input44, 30, {mc14_12, internal}},
     {fb01, input46, 30, {mc14_12, internal}},
     {fb01, input54, 30, {mc14_13, internal}},
     {fb01, input07, 31, {mc16_07, internal}},
     {fb01, input09, 31, {mc16_09, internal}},
     {fb01, input17, 31, {mc16_17, internal}},
     {fb01, input24, 31, {mc16_17, internal}},
     {fb01, input27, 31, {mc16_17, internal}},
     {fb01, input29, 31, {mc16_09, internal}},
     {fb01, input38, 31, {mc16_07, internal}},
     {fb01, input43, 31, {mc16_07, internal}},
     {fb01, input50, 31, {mc16_09, internal}}
    ];
results(_) ->
    [].

