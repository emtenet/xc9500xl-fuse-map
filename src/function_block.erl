-module(function_block).

-export([assert/1]).
-export([from/1]).
-export([name/1]).
-export([macro_cells/1]).

%%====================================================================
%% assert
%%====================================================================

assert(fb01) -> ok;
assert(fb02) -> ok;
assert(fb03) -> ok;
assert(fb04) -> ok;
assert(fb05) -> ok;
assert(fb06) -> ok;
assert(fb07) -> ok;
assert(fb08) -> ok;
assert(fb09) -> ok;
assert(fb10) -> ok;
assert(fb11) -> ok;
assert(fb12) -> ok;
assert(fb13) -> ok;
assert(fb14) -> ok;
assert(fb15) -> ok;
assert(fb16) -> ok.

%%====================================================================
%% from
%%====================================================================

from( 1) -> fb01;
from( 2) -> fb02;
from( 3) -> fb03;
from( 4) -> fb04;
from( 5) -> fb05;
from( 6) -> fb06;
from( 7) -> fb07;
from( 8) -> fb08;
from( 9) -> fb09;
from(10) -> fb10;
from(11) -> fb11;
from(12) -> fb12;
from(13) -> fb13;
from(14) -> fb14;
from(15) -> fb15;
from(16) -> fb16.

%%====================================================================
%% name
%%====================================================================

name(fb01) -> "FB1";
name(fb02) -> "FB2";
name(fb03) -> "FB3";
name(fb04) -> "FB4";
name(fb05) -> "FB5";
name(fb06) -> "FB6";
name(fb07) -> "FB7";
name(fb08) -> "FB8";
name(fb09) -> "FB9";
name(fb10) -> "FB10";
name(fb11) -> "FB11";
name(fb12) -> "FB12";
name(fb13) -> "FB13";
name(fb14) -> "FB14";
name(fb15) -> "FB15";
name(fb16) -> "FB16".

%%====================================================================
%% macro_cells
%%====================================================================

macro_cells(fb01) ->
    [mc01_01,
     mc01_02,
     mc01_03,
     mc01_04,
     mc01_05,
     mc01_06,
     mc01_07,
     mc01_08,
     mc01_09,
     mc01_10,
     mc01_11,
     mc01_12,
     mc01_13,
     mc01_14,
     mc01_15,
     mc01_16,
     mc01_17,
     mc01_18];
macro_cells(fb02) ->
    [mc02_01,
     mc02_02,
     mc02_03,
     mc02_04,
     mc02_05,
     mc02_06,
     mc02_07,
     mc02_08,
     mc02_09,
     mc02_10,
     mc02_11,
     mc02_12,
     mc02_13,
     mc02_14,
     mc02_15,
     mc02_16,
     mc02_17,
     mc02_18];
macro_cells(fb03) ->
    [mc03_01,
     mc03_02,
     mc03_03,
     mc03_04,
     mc03_05,
     mc03_06,
     mc03_07,
     mc03_08,
     mc03_09,
     mc03_10,
     mc03_11,
     mc03_12,
     mc03_13,
     mc03_14,
     mc03_15,
     mc03_16,
     mc03_17,
     mc03_18];
macro_cells(fb04) ->
    [mc04_01,
     mc04_02,
     mc04_03,
     mc04_04,
     mc04_05,
     mc04_06,
     mc04_07,
     mc04_08,
     mc04_09,
     mc04_10,
     mc04_11,
     mc04_12,
     mc04_13,
     mc04_14,
     mc04_15,
     mc04_16,
     mc04_17,
     mc04_18];
macro_cells(fb05) ->
    [mc05_01,
     mc05_02,
     mc05_03,
     mc05_04,
     mc05_05,
     mc05_06,
     mc05_07,
     mc05_08,
     mc05_09,
     mc05_10,
     mc05_11,
     mc05_12,
     mc05_13,
     mc05_14,
     mc05_15,
     mc05_16,
     mc05_17,
     mc05_18];
macro_cells(fb06) ->
    [mc06_01,
     mc06_02,
     mc06_03,
     mc06_04,
     mc06_05,
     mc06_06,
     mc06_07,
     mc06_08,
     mc06_09,
     mc06_10,
     mc06_11,
     mc06_12,
     mc06_13,
     mc06_14,
     mc06_15,
     mc06_16,
     mc06_17,
     mc06_18];
macro_cells(fb07) ->
    [mc07_01,
     mc07_02,
     mc07_03,
     mc07_04,
     mc07_05,
     mc07_06,
     mc07_07,
     mc07_08,
     mc07_09,
     mc07_10,
     mc07_11,
     mc07_12,
     mc07_13,
     mc07_14,
     mc07_15,
     mc07_16,
     mc07_17,
     mc07_18];
macro_cells(fb08) ->
    [mc08_01,
     mc08_02,
     mc08_03,
     mc08_04,
     mc08_05,
     mc08_06,
     mc08_07,
     mc08_08,
     mc08_09,
     mc08_10,
     mc08_11,
     mc08_12,
     mc08_13,
     mc08_14,
     mc08_15,
     mc08_16,
     mc08_17,
     mc08_18];
macro_cells(fb09) ->
    [mc09_01,
     mc09_02,
     mc09_03,
     mc09_04,
     mc09_05,
     mc09_06,
     mc09_07,
     mc09_08,
     mc09_09,
     mc09_10,
     mc09_11,
     mc09_12,
     mc09_13,
     mc09_14,
     mc09_15,
     mc09_16,
     mc09_17,
     mc09_18];
macro_cells(fb10) ->
    [mc10_01,
     mc10_02,
     mc10_03,
     mc10_04,
     mc10_05,
     mc10_06,
     mc10_07,
     mc10_08,
     mc10_09,
     mc10_10,
     mc10_11,
     mc10_12,
     mc10_13,
     mc10_14,
     mc10_15,
     mc10_16,
     mc10_17,
     mc10_18];
macro_cells(fb11) ->
    [mc11_01,
     mc11_02,
     mc11_03,
     mc11_04,
     mc11_05,
     mc11_06,
     mc11_07,
     mc11_08,
     mc11_09,
     mc11_10,
     mc11_11,
     mc11_12,
     mc11_13,
     mc11_14,
     mc11_15,
     mc11_16,
     mc11_17,
     mc11_18];
macro_cells(fb12) ->
    [mc12_01,
     mc12_02,
     mc12_03,
     mc12_04,
     mc12_05,
     mc12_06,
     mc12_07,
     mc12_08,
     mc12_09,
     mc12_10,
     mc12_11,
     mc12_12,
     mc12_13,
     mc12_14,
     mc12_15,
     mc12_16,
     mc12_17,
     mc12_18];
macro_cells(fb13) ->
    [mc13_01,
     mc13_02,
     mc13_03,
     mc13_04,
     mc13_05,
     mc13_06,
     mc13_07,
     mc13_08,
     mc13_09,
     mc13_10,
     mc13_11,
     mc13_12,
     mc13_13,
     mc13_14,
     mc13_15,
     mc13_16,
     mc13_17,
     mc13_18];
macro_cells(fb14) ->
    [mc14_01,
     mc14_02,
     mc14_03,
     mc14_04,
     mc14_05,
     mc14_06,
     mc14_07,
     mc14_08,
     mc14_09,
     mc14_10,
     mc14_11,
     mc14_12,
     mc14_13,
     mc14_14,
     mc14_15,
     mc14_16,
     mc14_17,
     mc14_18];
macro_cells(fb15) ->
    [mc15_01,
     mc15_02,
     mc15_03,
     mc15_04,
     mc15_05,
     mc15_06,
     mc15_07,
     mc15_08,
     mc15_09,
     mc15_10,
     mc15_11,
     mc15_12,
     mc15_13,
     mc15_14,
     mc15_15,
     mc15_16,
     mc15_17,
     mc15_18];
macro_cells(fb16) ->
    [mc16_01,
     mc16_02,
     mc16_03,
     mc16_04,
     mc16_05,
     mc16_06,
     mc16_07,
     mc16_08,
     mc16_09,
     mc16_10,
     mc16_11,
     mc16_12,
     mc16_13,
     mc16_14,
     mc16_15,
     mc16_16,
     mc16_17,
     mc16_18].

