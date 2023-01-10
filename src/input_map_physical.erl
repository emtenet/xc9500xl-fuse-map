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
results(_) ->
    [].

