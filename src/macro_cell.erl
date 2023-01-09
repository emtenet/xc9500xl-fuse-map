-module(macro_cell).

-export([list/0]).
-export([from/2]).
-export([name/1]).
-export([function_block/1]).
-export([split/1]).
-export([join/2]).

-export_type([macro_cell/0]).
-export_type([absolute/0]).

-type function_block() :: function_block:function_block().

-type macro_cell() ::
    mc01 |
    mc02 |
    mc03 |
    mc04 |
    mc05 |
    mc06 |
    mc07 |
    mc08 |
    mc09 |
    mc10 |
    mc11 |
    mc12 |
    mc13 |
    mc14 |
    mc15 |
    mc16 |
    mc17 |
    mc18.

-type absolute() ::
    mc01_01 | mc01_02 | mc01_03 | mc01_04 | mc01_05 | mc01_06 | 
    mc01_07 | mc01_08 | mc01_09 | mc01_10 | mc01_11 | mc01_12 | 
    mc01_13 | mc01_14 | mc01_15 | mc01_16 | mc01_17 | mc01_18 | 
    mc02_01 | mc02_02 | mc02_03 | mc02_04 | mc02_05 | mc02_06 | 
    mc02_07 | mc02_08 | mc02_09 | mc02_10 | mc02_11 | mc02_12 | 
    mc02_13 | mc02_14 | mc02_15 | mc02_16 | mc02_17 | mc02_18 | 
    mc03_01 | mc03_02 | mc03_03 | mc03_04 | mc03_05 | mc03_06 | 
    mc03_07 | mc03_08 | mc03_09 | mc03_10 | mc03_11 | mc03_12 | 
    mc03_13 | mc03_14 | mc03_15 | mc03_16 | mc03_17 | mc03_18 | 
    mc04_01 | mc04_02 | mc04_03 | mc04_04 | mc04_05 | mc04_06 | 
    mc04_07 | mc04_08 | mc04_09 | mc04_10 | mc04_11 | mc04_12 | 
    mc04_13 | mc04_14 | mc04_15 | mc04_16 | mc04_17 | mc04_18 | 
    mc05_01 | mc05_02 | mc05_03 | mc05_04 | mc05_05 | mc05_06 | 
    mc05_07 | mc05_08 | mc05_09 | mc05_10 | mc05_11 | mc05_12 | 
    mc05_13 | mc05_14 | mc05_15 | mc05_16 | mc05_17 | mc05_18 | 
    mc06_01 | mc06_02 | mc06_03 | mc06_04 | mc06_05 | mc06_06 | 
    mc06_07 | mc06_08 | mc06_09 | mc06_10 | mc06_11 | mc06_12 | 
    mc06_13 | mc06_14 | mc06_15 | mc06_16 | mc06_17 | mc06_18 | 
    mc07_01 | mc07_02 | mc07_03 | mc07_04 | mc07_05 | mc07_06 | 
    mc07_07 | mc07_08 | mc07_09 | mc07_10 | mc07_11 | mc07_12 | 
    mc07_13 | mc07_14 | mc07_15 | mc07_16 | mc07_17 | mc07_18 | 
    mc08_01 | mc08_02 | mc08_03 | mc08_04 | mc08_05 | mc08_06 | 
    mc08_07 | mc08_08 | mc08_09 | mc08_10 | mc08_11 | mc08_12 | 
    mc08_13 | mc08_14 | mc08_15 | mc08_16 | mc08_17 | mc08_18 | 
    mc09_01 | mc09_02 | mc09_03 | mc09_04 | mc09_05 | mc09_06 | 
    mc09_07 | mc09_08 | mc09_09 | mc09_10 | mc09_11 | mc09_12 | 
    mc09_13 | mc09_14 | mc09_15 | mc09_16 | mc09_17 | mc09_18 | 
    mc10_01 | mc10_02 | mc10_03 | mc10_04 | mc10_05 | mc10_06 | 
    mc10_07 | mc10_08 | mc10_09 | mc10_10 | mc10_11 | mc10_12 | 
    mc10_13 | mc10_14 | mc10_15 | mc10_16 | mc10_17 | mc10_18 | 
    mc11_01 | mc11_02 | mc11_03 | mc11_04 | mc11_05 | mc11_06 | 
    mc11_07 | mc11_08 | mc11_09 | mc11_10 | mc11_11 | mc11_12 | 
    mc11_13 | mc11_14 | mc11_15 | mc11_16 | mc11_17 | mc11_18 | 
    mc12_01 | mc12_02 | mc12_03 | mc12_04 | mc12_05 | mc12_06 | 
    mc12_07 | mc12_08 | mc12_09 | mc12_10 | mc12_11 | mc12_12 | 
    mc12_13 | mc12_14 | mc12_15 | mc12_16 | mc12_17 | mc12_18 | 
    mc13_01 | mc13_02 | mc13_03 | mc13_04 | mc13_05 | mc13_06 | 
    mc13_07 | mc13_08 | mc13_09 | mc13_10 | mc13_11 | mc13_12 | 
    mc13_13 | mc13_14 | mc13_15 | mc13_16 | mc13_17 | mc13_18 | 
    mc14_01 | mc14_02 | mc14_03 | mc14_04 | mc14_05 | mc14_06 | 
    mc14_07 | mc14_08 | mc14_09 | mc14_10 | mc14_11 | mc14_12 | 
    mc14_13 | mc14_14 | mc14_15 | mc14_16 | mc14_17 | mc14_18 | 
    mc15_01 | mc15_02 | mc15_03 | mc15_04 | mc15_05 | mc15_06 | 
    mc15_07 | mc15_08 | mc15_09 | mc15_10 | mc15_11 | mc15_12 | 
    mc15_13 | mc15_14 | mc15_15 | mc15_16 | mc15_17 | mc15_18 | 
    mc16_01 | mc16_02 | mc16_03 | mc16_04 | mc16_05 | mc16_06 | 
    mc16_07 | mc16_08 | mc16_09 | mc16_10 | mc16_11 | mc16_12 | 
    mc16_13 | mc16_14 | mc16_15 | mc16_16 | mc16_17 | mc16_18.

%%====================================================================
%% list
%%====================================================================

-spec list() -> [macro_cell()].

list() ->
    [mc01,
     mc02,
     mc03,
     mc04,
     mc05,
     mc06,
     mc07,
     mc08,
     mc09,
     mc10,
     mc11,
     mc12,
     mc13,
     mc14,
     mc15,
     mc16,
     mc17,
     mc18].

%%====================================================================
%% from
%%====================================================================

-spec from(1..16, 1..18) -> absolute().

from( 1,  1) -> mc01_01;
from( 1,  2) -> mc01_02;
from( 1,  3) -> mc01_03;
from( 1,  4) -> mc01_04;
from( 1,  5) -> mc01_05;
from( 1,  6) -> mc01_06;
from( 1,  7) -> mc01_07;
from( 1,  8) -> mc01_08;
from( 1,  9) -> mc01_09;
from( 1, 10) -> mc01_10;
from( 1, 11) -> mc01_11;
from( 1, 12) -> mc01_12;
from( 1, 13) -> mc01_13;
from( 1, 14) -> mc01_14;
from( 1, 15) -> mc01_15;
from( 1, 16) -> mc01_16;
from( 1, 17) -> mc01_17;
from( 1, 18) -> mc01_18;
from( 2,  1) -> mc02_01;
from( 2,  2) -> mc02_02;
from( 2,  3) -> mc02_03;
from( 2,  4) -> mc02_04;
from( 2,  5) -> mc02_05;
from( 2,  6) -> mc02_06;
from( 2,  7) -> mc02_07;
from( 2,  8) -> mc02_08;
from( 2,  9) -> mc02_09;
from( 2, 10) -> mc02_10;
from( 2, 11) -> mc02_11;
from( 2, 12) -> mc02_12;
from( 2, 13) -> mc02_13;
from( 2, 14) -> mc02_14;
from( 2, 15) -> mc02_15;
from( 2, 16) -> mc02_16;
from( 2, 17) -> mc02_17;
from( 2, 18) -> mc02_18;
from( 3,  1) -> mc03_01;
from( 3,  2) -> mc03_02;
from( 3,  3) -> mc03_03;
from( 3,  4) -> mc03_04;
from( 3,  5) -> mc03_05;
from( 3,  6) -> mc03_06;
from( 3,  7) -> mc03_07;
from( 3,  8) -> mc03_08;
from( 3,  9) -> mc03_09;
from( 3, 10) -> mc03_10;
from( 3, 11) -> mc03_11;
from( 3, 12) -> mc03_12;
from( 3, 13) -> mc03_13;
from( 3, 14) -> mc03_14;
from( 3, 15) -> mc03_15;
from( 3, 16) -> mc03_16;
from( 3, 17) -> mc03_17;
from( 3, 18) -> mc03_18;
from( 4,  1) -> mc04_01;
from( 4,  2) -> mc04_02;
from( 4,  3) -> mc04_03;
from( 4,  4) -> mc04_04;
from( 4,  5) -> mc04_05;
from( 4,  6) -> mc04_06;
from( 4,  7) -> mc04_07;
from( 4,  8) -> mc04_08;
from( 4,  9) -> mc04_09;
from( 4, 10) -> mc04_10;
from( 4, 11) -> mc04_11;
from( 4, 12) -> mc04_12;
from( 4, 13) -> mc04_13;
from( 4, 14) -> mc04_14;
from( 4, 15) -> mc04_15;
from( 4, 16) -> mc04_16;
from( 4, 17) -> mc04_17;
from( 4, 18) -> mc04_18;
from( 5,  1) -> mc05_01;
from( 5,  2) -> mc05_02;
from( 5,  3) -> mc05_03;
from( 5,  4) -> mc05_04;
from( 5,  5) -> mc05_05;
from( 5,  6) -> mc05_06;
from( 5,  7) -> mc05_07;
from( 5,  8) -> mc05_08;
from( 5,  9) -> mc05_09;
from( 5, 10) -> mc05_10;
from( 5, 11) -> mc05_11;
from( 5, 12) -> mc05_12;
from( 5, 13) -> mc05_13;
from( 5, 14) -> mc05_14;
from( 5, 15) -> mc05_15;
from( 5, 16) -> mc05_16;
from( 5, 17) -> mc05_17;
from( 5, 18) -> mc05_18;
from( 6,  1) -> mc06_01;
from( 6,  2) -> mc06_02;
from( 6,  3) -> mc06_03;
from( 6,  4) -> mc06_04;
from( 6,  5) -> mc06_05;
from( 6,  6) -> mc06_06;
from( 6,  7) -> mc06_07;
from( 6,  8) -> mc06_08;
from( 6,  9) -> mc06_09;
from( 6, 10) -> mc06_10;
from( 6, 11) -> mc06_11;
from( 6, 12) -> mc06_12;
from( 6, 13) -> mc06_13;
from( 6, 14) -> mc06_14;
from( 6, 15) -> mc06_15;
from( 6, 16) -> mc06_16;
from( 6, 17) -> mc06_17;
from( 6, 18) -> mc06_18;
from( 7,  1) -> mc07_01;
from( 7,  2) -> mc07_02;
from( 7,  3) -> mc07_03;
from( 7,  4) -> mc07_04;
from( 7,  5) -> mc07_05;
from( 7,  6) -> mc07_06;
from( 7,  7) -> mc07_07;
from( 7,  8) -> mc07_08;
from( 7,  9) -> mc07_09;
from( 7, 10) -> mc07_10;
from( 7, 11) -> mc07_11;
from( 7, 12) -> mc07_12;
from( 7, 13) -> mc07_13;
from( 7, 14) -> mc07_14;
from( 7, 15) -> mc07_15;
from( 7, 16) -> mc07_16;
from( 7, 17) -> mc07_17;
from( 7, 18) -> mc07_18;
from( 8,  1) -> mc08_01;
from( 8,  2) -> mc08_02;
from( 8,  3) -> mc08_03;
from( 8,  4) -> mc08_04;
from( 8,  5) -> mc08_05;
from( 8,  6) -> mc08_06;
from( 8,  7) -> mc08_07;
from( 8,  8) -> mc08_08;
from( 8,  9) -> mc08_09;
from( 8, 10) -> mc08_10;
from( 8, 11) -> mc08_11;
from( 8, 12) -> mc08_12;
from( 8, 13) -> mc08_13;
from( 8, 14) -> mc08_14;
from( 8, 15) -> mc08_15;
from( 8, 16) -> mc08_16;
from( 8, 17) -> mc08_17;
from( 8, 18) -> mc08_18;
from( 9,  1) -> mc09_01;
from( 9,  2) -> mc09_02;
from( 9,  3) -> mc09_03;
from( 9,  4) -> mc09_04;
from( 9,  5) -> mc09_05;
from( 9,  6) -> mc09_06;
from( 9,  7) -> mc09_07;
from( 9,  8) -> mc09_08;
from( 9,  9) -> mc09_09;
from( 9, 10) -> mc09_10;
from( 9, 11) -> mc09_11;
from( 9, 12) -> mc09_12;
from( 9, 13) -> mc09_13;
from( 9, 14) -> mc09_14;
from( 9, 15) -> mc09_15;
from( 9, 16) -> mc09_16;
from( 9, 17) -> mc09_17;
from( 9, 18) -> mc09_18;
from(10,  1) -> mc10_01;
from(10,  2) -> mc10_02;
from(10,  3) -> mc10_03;
from(10,  4) -> mc10_04;
from(10,  5) -> mc10_05;
from(10,  6) -> mc10_06;
from(10,  7) -> mc10_07;
from(10,  8) -> mc10_08;
from(10,  9) -> mc10_09;
from(10, 10) -> mc10_10;
from(10, 11) -> mc10_11;
from(10, 12) -> mc10_12;
from(10, 13) -> mc10_13;
from(10, 14) -> mc10_14;
from(10, 15) -> mc10_15;
from(10, 16) -> mc10_16;
from(10, 17) -> mc10_17;
from(10, 18) -> mc10_18;
from(11,  1) -> mc11_01;
from(11,  2) -> mc11_02;
from(11,  3) -> mc11_03;
from(11,  4) -> mc11_04;
from(11,  5) -> mc11_05;
from(11,  6) -> mc11_06;
from(11,  7) -> mc11_07;
from(11,  8) -> mc11_08;
from(11,  9) -> mc11_09;
from(11, 10) -> mc11_10;
from(11, 11) -> mc11_11;
from(11, 12) -> mc11_12;
from(11, 13) -> mc11_13;
from(11, 14) -> mc11_14;
from(11, 15) -> mc11_15;
from(11, 16) -> mc11_16;
from(11, 17) -> mc11_17;
from(11, 18) -> mc11_18;
from(12,  1) -> mc12_01;
from(12,  2) -> mc12_02;
from(12,  3) -> mc12_03;
from(12,  4) -> mc12_04;
from(12,  5) -> mc12_05;
from(12,  6) -> mc12_06;
from(12,  7) -> mc12_07;
from(12,  8) -> mc12_08;
from(12,  9) -> mc12_09;
from(12, 10) -> mc12_10;
from(12, 11) -> mc12_11;
from(12, 12) -> mc12_12;
from(12, 13) -> mc12_13;
from(12, 14) -> mc12_14;
from(12, 15) -> mc12_15;
from(12, 16) -> mc12_16;
from(12, 17) -> mc12_17;
from(12, 18) -> mc12_18;
from(13,  1) -> mc13_01;
from(13,  2) -> mc13_02;
from(13,  3) -> mc13_03;
from(13,  4) -> mc13_04;
from(13,  5) -> mc13_05;
from(13,  6) -> mc13_06;
from(13,  7) -> mc13_07;
from(13,  8) -> mc13_08;
from(13,  9) -> mc13_09;
from(13, 10) -> mc13_10;
from(13, 11) -> mc13_11;
from(13, 12) -> mc13_12;
from(13, 13) -> mc13_13;
from(13, 14) -> mc13_14;
from(13, 15) -> mc13_15;
from(13, 16) -> mc13_16;
from(13, 17) -> mc13_17;
from(13, 18) -> mc13_18;
from(14,  1) -> mc14_01;
from(14,  2) -> mc14_02;
from(14,  3) -> mc14_03;
from(14,  4) -> mc14_04;
from(14,  5) -> mc14_05;
from(14,  6) -> mc14_06;
from(14,  7) -> mc14_07;
from(14,  8) -> mc14_08;
from(14,  9) -> mc14_09;
from(14, 10) -> mc14_10;
from(14, 11) -> mc14_11;
from(14, 12) -> mc14_12;
from(14, 13) -> mc14_13;
from(14, 14) -> mc14_14;
from(14, 15) -> mc14_15;
from(14, 16) -> mc14_16;
from(14, 17) -> mc14_17;
from(14, 18) -> mc14_18;
from(15,  1) -> mc15_01;
from(15,  2) -> mc15_02;
from(15,  3) -> mc15_03;
from(15,  4) -> mc15_04;
from(15,  5) -> mc15_05;
from(15,  6) -> mc15_06;
from(15,  7) -> mc15_07;
from(15,  8) -> mc15_08;
from(15,  9) -> mc15_09;
from(15, 10) -> mc15_10;
from(15, 11) -> mc15_11;
from(15, 12) -> mc15_12;
from(15, 13) -> mc15_13;
from(15, 14) -> mc15_14;
from(15, 15) -> mc15_15;
from(15, 16) -> mc15_16;
from(15, 17) -> mc15_17;
from(15, 18) -> mc15_18;
from(16,  1) -> mc16_01;
from(16,  2) -> mc16_02;
from(16,  3) -> mc16_03;
from(16,  4) -> mc16_04;
from(16,  5) -> mc16_05;
from(16,  6) -> mc16_06;
from(16,  7) -> mc16_07;
from(16,  8) -> mc16_08;
from(16,  9) -> mc16_09;
from(16, 10) -> mc16_10;
from(16, 11) -> mc16_11;
from(16, 12) -> mc16_12;
from(16, 13) -> mc16_13;
from(16, 14) -> mc16_14;
from(16, 15) -> mc16_15;
from(16, 16) -> mc16_16;
from(16, 17) -> mc16_17;
from(16, 18) -> mc16_18.

%%====================;
%% name
%%====================;

-spec name(absolute()) -> binary().

name(mc01_01) -> <<"FB1_1">>;
name(mc01_02) -> <<"FB1_2">>;
name(mc01_03) -> <<"FB1_3">>;
name(mc01_04) -> <<"FB1_4">>;
name(mc01_05) -> <<"FB1_5">>;
name(mc01_06) -> <<"FB1_6">>;
name(mc01_07) -> <<"FB1_7">>;
name(mc01_08) -> <<"FB1_8">>;
name(mc01_09) -> <<"FB1_9">>;
name(mc01_10) -> <<"FB1_10">>;
name(mc01_11) -> <<"FB1_11">>;
name(mc01_12) -> <<"FB1_12">>;
name(mc01_13) -> <<"FB1_13">>;
name(mc01_14) -> <<"FB1_14">>;
name(mc01_15) -> <<"FB1_15">>;
name(mc01_16) -> <<"FB1_16">>;
name(mc01_17) -> <<"FB1_17">>;
name(mc01_18) -> <<"FB1_18">>;
name(mc02_01) -> <<"FB2_1">>;
name(mc02_02) -> <<"FB2_2">>;
name(mc02_03) -> <<"FB2_3">>;
name(mc02_04) -> <<"FB2_4">>;
name(mc02_05) -> <<"FB2_5">>;
name(mc02_06) -> <<"FB2_6">>;
name(mc02_07) -> <<"FB2_7">>;
name(mc02_08) -> <<"FB2_8">>;
name(mc02_09) -> <<"FB2_9">>;
name(mc02_10) -> <<"FB2_10">>;
name(mc02_11) -> <<"FB2_11">>;
name(mc02_12) -> <<"FB2_12">>;
name(mc02_13) -> <<"FB2_13">>;
name(mc02_14) -> <<"FB2_14">>;
name(mc02_15) -> <<"FB2_15">>;
name(mc02_16) -> <<"FB2_16">>;
name(mc02_17) -> <<"FB2_17">>;
name(mc02_18) -> <<"FB2_18">>;
name(mc03_01) -> <<"FB3_1">>;
name(mc03_02) -> <<"FB3_2">>;
name(mc03_03) -> <<"FB3_3">>;
name(mc03_04) -> <<"FB3_4">>;
name(mc03_05) -> <<"FB3_5">>;
name(mc03_06) -> <<"FB3_6">>;
name(mc03_07) -> <<"FB3_7">>;
name(mc03_08) -> <<"FB3_8">>;
name(mc03_09) -> <<"FB3_9">>;
name(mc03_10) -> <<"FB3_10">>;
name(mc03_11) -> <<"FB3_11">>;
name(mc03_12) -> <<"FB3_12">>;
name(mc03_13) -> <<"FB3_13">>;
name(mc03_14) -> <<"FB3_14">>;
name(mc03_15) -> <<"FB3_15">>;
name(mc03_16) -> <<"FB3_16">>;
name(mc03_17) -> <<"FB3_17">>;
name(mc03_18) -> <<"FB3_18">>;
name(mc04_01) -> <<"FB4_1">>;
name(mc04_02) -> <<"FB4_2">>;
name(mc04_03) -> <<"FB4_3">>;
name(mc04_04) -> <<"FB4_4">>;
name(mc04_05) -> <<"FB4_5">>;
name(mc04_06) -> <<"FB4_6">>;
name(mc04_07) -> <<"FB4_7">>;
name(mc04_08) -> <<"FB4_8">>;
name(mc04_09) -> <<"FB4_9">>;
name(mc04_10) -> <<"FB4_10">>;
name(mc04_11) -> <<"FB4_11">>;
name(mc04_12) -> <<"FB4_12">>;
name(mc04_13) -> <<"FB4_13">>;
name(mc04_14) -> <<"FB4_14">>;
name(mc04_15) -> <<"FB4_15">>;
name(mc04_16) -> <<"FB4_16">>;
name(mc04_17) -> <<"FB4_17">>;
name(mc04_18) -> <<"FB4_18">>;
name(mc05_01) -> <<"FB5_1">>;
name(mc05_02) -> <<"FB5_2">>;
name(mc05_03) -> <<"FB5_3">>;
name(mc05_04) -> <<"FB5_4">>;
name(mc05_05) -> <<"FB5_5">>;
name(mc05_06) -> <<"FB5_6">>;
name(mc05_07) -> <<"FB5_7">>;
name(mc05_08) -> <<"FB5_8">>;
name(mc05_09) -> <<"FB5_9">>;
name(mc05_10) -> <<"FB5_10">>;
name(mc05_11) -> <<"FB5_11">>;
name(mc05_12) -> <<"FB5_12">>;
name(mc05_13) -> <<"FB5_13">>;
name(mc05_14) -> <<"FB5_14">>;
name(mc05_15) -> <<"FB5_15">>;
name(mc05_16) -> <<"FB5_16">>;
name(mc05_17) -> <<"FB5_17">>;
name(mc05_18) -> <<"FB5_18">>;
name(mc06_01) -> <<"FB6_1">>;
name(mc06_02) -> <<"FB6_2">>;
name(mc06_03) -> <<"FB6_3">>;
name(mc06_04) -> <<"FB6_4">>;
name(mc06_05) -> <<"FB6_5">>;
name(mc06_06) -> <<"FB6_6">>;
name(mc06_07) -> <<"FB6_7">>;
name(mc06_08) -> <<"FB6_8">>;
name(mc06_09) -> <<"FB6_9">>;
name(mc06_10) -> <<"FB6_10">>;
name(mc06_11) -> <<"FB6_11">>;
name(mc06_12) -> <<"FB6_12">>;
name(mc06_13) -> <<"FB6_13">>;
name(mc06_14) -> <<"FB6_14">>;
name(mc06_15) -> <<"FB6_15">>;
name(mc06_16) -> <<"FB6_16">>;
name(mc06_17) -> <<"FB6_17">>;
name(mc06_18) -> <<"FB6_18">>;
name(mc07_01) -> <<"FB7_1">>;
name(mc07_02) -> <<"FB7_2">>;
name(mc07_03) -> <<"FB7_3">>;
name(mc07_04) -> <<"FB7_4">>;
name(mc07_05) -> <<"FB7_5">>;
name(mc07_06) -> <<"FB7_6">>;
name(mc07_07) -> <<"FB7_7">>;
name(mc07_08) -> <<"FB7_8">>;
name(mc07_09) -> <<"FB7_9">>;
name(mc07_10) -> <<"FB7_10">>;
name(mc07_11) -> <<"FB7_11">>;
name(mc07_12) -> <<"FB7_12">>;
name(mc07_13) -> <<"FB7_13">>;
name(mc07_14) -> <<"FB7_14">>;
name(mc07_15) -> <<"FB7_15">>;
name(mc07_16) -> <<"FB7_16">>;
name(mc07_17) -> <<"FB7_17">>;
name(mc07_18) -> <<"FB7_18">>;
name(mc08_01) -> <<"FB8_1">>;
name(mc08_02) -> <<"FB8_2">>;
name(mc08_03) -> <<"FB8_3">>;
name(mc08_04) -> <<"FB8_4">>;
name(mc08_05) -> <<"FB8_5">>;
name(mc08_06) -> <<"FB8_6">>;
name(mc08_07) -> <<"FB8_7">>;
name(mc08_08) -> <<"FB8_8">>;
name(mc08_09) -> <<"FB8_9">>;
name(mc08_10) -> <<"FB8_10">>;
name(mc08_11) -> <<"FB8_11">>;
name(mc08_12) -> <<"FB8_12">>;
name(mc08_13) -> <<"FB8_13">>;
name(mc08_14) -> <<"FB8_14">>;
name(mc08_15) -> <<"FB8_15">>;
name(mc08_16) -> <<"FB8_16">>;
name(mc08_17) -> <<"FB8_17">>;
name(mc08_18) -> <<"FB8_18">>;
name(mc09_01) -> <<"FB9_1">>;
name(mc09_02) -> <<"FB9_2">>;
name(mc09_03) -> <<"FB9_3">>;
name(mc09_04) -> <<"FB9_4">>;
name(mc09_05) -> <<"FB9_5">>;
name(mc09_06) -> <<"FB9_6">>;
name(mc09_07) -> <<"FB9_7">>;
name(mc09_08) -> <<"FB9_8">>;
name(mc09_09) -> <<"FB9_9">>;
name(mc09_10) -> <<"FB9_10">>;
name(mc09_11) -> <<"FB9_11">>;
name(mc09_12) -> <<"FB9_12">>;
name(mc09_13) -> <<"FB9_13">>;
name(mc09_14) -> <<"FB9_14">>;
name(mc09_15) -> <<"FB9_15">>;
name(mc09_16) -> <<"FB9_16">>;
name(mc09_17) -> <<"FB9_17">>;
name(mc09_18) -> <<"FB9_18">>;
name(mc10_01) -> <<"FB10_1">>;
name(mc10_02) -> <<"FB10_2">>;
name(mc10_03) -> <<"FB10_3">>;
name(mc10_04) -> <<"FB10_4">>;
name(mc10_05) -> <<"FB10_5">>;
name(mc10_06) -> <<"FB10_6">>;
name(mc10_07) -> <<"FB10_7">>;
name(mc10_08) -> <<"FB10_8">>;
name(mc10_09) -> <<"FB10_9">>;
name(mc10_10) -> <<"FB10_10">>;
name(mc10_11) -> <<"FB10_11">>;
name(mc10_12) -> <<"FB10_12">>;
name(mc10_13) -> <<"FB10_13">>;
name(mc10_14) -> <<"FB10_14">>;
name(mc10_15) -> <<"FB10_15">>;
name(mc10_16) -> <<"FB10_16">>;
name(mc10_17) -> <<"FB10_17">>;
name(mc10_18) -> <<"FB10_18">>;
name(mc11_01) -> <<"FB11_1">>;
name(mc11_02) -> <<"FB11_2">>;
name(mc11_03) -> <<"FB11_3">>;
name(mc11_04) -> <<"FB11_4">>;
name(mc11_05) -> <<"FB11_5">>;
name(mc11_06) -> <<"FB11_6">>;
name(mc11_07) -> <<"FB11_7">>;
name(mc11_08) -> <<"FB11_8">>;
name(mc11_09) -> <<"FB11_9">>;
name(mc11_10) -> <<"FB11_10">>;
name(mc11_11) -> <<"FB11_11">>;
name(mc11_12) -> <<"FB11_12">>;
name(mc11_13) -> <<"FB11_13">>;
name(mc11_14) -> <<"FB11_14">>;
name(mc11_15) -> <<"FB11_15">>;
name(mc11_16) -> <<"FB11_16">>;
name(mc11_17) -> <<"FB11_17">>;
name(mc11_18) -> <<"FB11_18">>;
name(mc12_01) -> <<"FB12_1">>;
name(mc12_02) -> <<"FB12_2">>;
name(mc12_03) -> <<"FB12_3">>;
name(mc12_04) -> <<"FB12_4">>;
name(mc12_05) -> <<"FB12_5">>;
name(mc12_06) -> <<"FB12_6">>;
name(mc12_07) -> <<"FB12_7">>;
name(mc12_08) -> <<"FB12_8">>;
name(mc12_09) -> <<"FB12_9">>;
name(mc12_10) -> <<"FB12_10">>;
name(mc12_11) -> <<"FB12_11">>;
name(mc12_12) -> <<"FB12_12">>;
name(mc12_13) -> <<"FB12_13">>;
name(mc12_14) -> <<"FB12_14">>;
name(mc12_15) -> <<"FB12_15">>;
name(mc12_16) -> <<"FB12_16">>;
name(mc12_17) -> <<"FB12_17">>;
name(mc12_18) -> <<"FB12_18">>;
name(mc13_01) -> <<"FB13_1">>;
name(mc13_02) -> <<"FB13_2">>;
name(mc13_03) -> <<"FB13_3">>;
name(mc13_04) -> <<"FB13_4">>;
name(mc13_05) -> <<"FB13_5">>;
name(mc13_06) -> <<"FB13_6">>;
name(mc13_07) -> <<"FB13_7">>;
name(mc13_08) -> <<"FB13_8">>;
name(mc13_09) -> <<"FB13_9">>;
name(mc13_10) -> <<"FB13_10">>;
name(mc13_11) -> <<"FB13_11">>;
name(mc13_12) -> <<"FB13_12">>;
name(mc13_13) -> <<"FB13_13">>;
name(mc13_14) -> <<"FB13_14">>;
name(mc13_15) -> <<"FB13_15">>;
name(mc13_16) -> <<"FB13_16">>;
name(mc13_17) -> <<"FB13_17">>;
name(mc13_18) -> <<"FB13_18">>;
name(mc14_01) -> <<"FB14_1">>;
name(mc14_02) -> <<"FB14_2">>;
name(mc14_03) -> <<"FB14_3">>;
name(mc14_04) -> <<"FB14_4">>;
name(mc14_05) -> <<"FB14_5">>;
name(mc14_06) -> <<"FB14_6">>;
name(mc14_07) -> <<"FB14_7">>;
name(mc14_08) -> <<"FB14_8">>;
name(mc14_09) -> <<"FB14_9">>;
name(mc14_10) -> <<"FB14_10">>;
name(mc14_11) -> <<"FB14_11">>;
name(mc14_12) -> <<"FB14_12">>;
name(mc14_13) -> <<"FB14_13">>;
name(mc14_14) -> <<"FB14_14">>;
name(mc14_15) -> <<"FB14_15">>;
name(mc14_16) -> <<"FB14_16">>;
name(mc14_17) -> <<"FB14_17">>;
name(mc14_18) -> <<"FB14_18">>;
name(mc15_01) -> <<"FB15_1">>;
name(mc15_02) -> <<"FB15_2">>;
name(mc15_03) -> <<"FB15_3">>;
name(mc15_04) -> <<"FB15_4">>;
name(mc15_05) -> <<"FB15_5">>;
name(mc15_06) -> <<"FB15_6">>;
name(mc15_07) -> <<"FB15_7">>;
name(mc15_08) -> <<"FB15_8">>;
name(mc15_09) -> <<"FB15_9">>;
name(mc15_10) -> <<"FB15_10">>;
name(mc15_11) -> <<"FB15_11">>;
name(mc15_12) -> <<"FB15_12">>;
name(mc15_13) -> <<"FB15_13">>;
name(mc15_14) -> <<"FB15_14">>;
name(mc15_15) -> <<"FB15_15">>;
name(mc15_16) -> <<"FB15_16">>;
name(mc15_17) -> <<"FB15_17">>;
name(mc15_18) -> <<"FB15_18">>;
name(mc16_01) -> <<"FB16_1">>;
name(mc16_02) -> <<"FB16_2">>;
name(mc16_03) -> <<"FB16_3">>;
name(mc16_04) -> <<"FB16_4">>;
name(mc16_05) -> <<"FB16_5">>;
name(mc16_06) -> <<"FB16_6">>;
name(mc16_07) -> <<"FB16_7">>;
name(mc16_08) -> <<"FB16_8">>;
name(mc16_09) -> <<"FB16_9">>;
name(mc16_10) -> <<"FB16_10">>;
name(mc16_11) -> <<"FB16_11">>;
name(mc16_12) -> <<"FB16_12">>;
name(mc16_13) -> <<"FB16_13">>;
name(mc16_14) -> <<"FB16_14">>;
name(mc16_15) -> <<"FB16_15">>;
name(mc16_16) -> <<"FB16_16">>;
name(mc16_17) -> <<"FB16_17">>;
name(mc16_18) -> <<"FB16_18">>.

%%====================================================================
%% function_block
%%====================================================================

-spec function_block(absolute()) -> function_block().

function_block(MC) ->
    {FB, _} = split(MC),
    FB.

%%====================================================================
%% split
%%====================================================================

-spec split(absolute()) -> {function_block(), macro_cell()}.

split(mc01_01) -> {fb01, mc01};
split(mc01_02) -> {fb01, mc02};
split(mc01_03) -> {fb01, mc03};
split(mc01_04) -> {fb01, mc04};
split(mc01_05) -> {fb01, mc05};
split(mc01_06) -> {fb01, mc06};
split(mc01_07) -> {fb01, mc07};
split(mc01_08) -> {fb01, mc08};
split(mc01_09) -> {fb01, mc09};
split(mc01_10) -> {fb01, mc10};
split(mc01_11) -> {fb01, mc11};
split(mc01_12) -> {fb01, mc12};
split(mc01_13) -> {fb01, mc13};
split(mc01_14) -> {fb01, mc14};
split(mc01_15) -> {fb01, mc15};
split(mc01_16) -> {fb01, mc16};
split(mc01_17) -> {fb01, mc17};
split(mc01_18) -> {fb01, mc18};
split(mc02_01) -> {fb02, mc01};
split(mc02_02) -> {fb02, mc02};
split(mc02_03) -> {fb02, mc03};
split(mc02_04) -> {fb02, mc04};
split(mc02_05) -> {fb02, mc05};
split(mc02_06) -> {fb02, mc06};
split(mc02_07) -> {fb02, mc07};
split(mc02_08) -> {fb02, mc08};
split(mc02_09) -> {fb02, mc09};
split(mc02_10) -> {fb02, mc10};
split(mc02_11) -> {fb02, mc11};
split(mc02_12) -> {fb02, mc12};
split(mc02_13) -> {fb02, mc13};
split(mc02_14) -> {fb02, mc14};
split(mc02_15) -> {fb02, mc15};
split(mc02_16) -> {fb02, mc16};
split(mc02_17) -> {fb02, mc17};
split(mc02_18) -> {fb02, mc18};
split(mc03_01) -> {fb03, mc01};
split(mc03_02) -> {fb03, mc02};
split(mc03_03) -> {fb03, mc03};
split(mc03_04) -> {fb03, mc04};
split(mc03_05) -> {fb03, mc05};
split(mc03_06) -> {fb03, mc06};
split(mc03_07) -> {fb03, mc07};
split(mc03_08) -> {fb03, mc08};
split(mc03_09) -> {fb03, mc09};
split(mc03_10) -> {fb03, mc10};
split(mc03_11) -> {fb03, mc11};
split(mc03_12) -> {fb03, mc12};
split(mc03_13) -> {fb03, mc13};
split(mc03_14) -> {fb03, mc14};
split(mc03_15) -> {fb03, mc15};
split(mc03_16) -> {fb03, mc16};
split(mc03_17) -> {fb03, mc17};
split(mc03_18) -> {fb03, mc18};
split(mc04_01) -> {fb04, mc01};
split(mc04_02) -> {fb04, mc02};
split(mc04_03) -> {fb04, mc03};
split(mc04_04) -> {fb04, mc04};
split(mc04_05) -> {fb04, mc05};
split(mc04_06) -> {fb04, mc06};
split(mc04_07) -> {fb04, mc07};
split(mc04_08) -> {fb04, mc08};
split(mc04_09) -> {fb04, mc09};
split(mc04_10) -> {fb04, mc10};
split(mc04_11) -> {fb04, mc11};
split(mc04_12) -> {fb04, mc12};
split(mc04_13) -> {fb04, mc13};
split(mc04_14) -> {fb04, mc14};
split(mc04_15) -> {fb04, mc15};
split(mc04_16) -> {fb04, mc16};
split(mc04_17) -> {fb04, mc17};
split(mc04_18) -> {fb04, mc18};
split(mc05_01) -> {fb05, mc01};
split(mc05_02) -> {fb05, mc02};
split(mc05_03) -> {fb05, mc03};
split(mc05_04) -> {fb05, mc04};
split(mc05_05) -> {fb05, mc05};
split(mc05_06) -> {fb05, mc06};
split(mc05_07) -> {fb05, mc07};
split(mc05_08) -> {fb05, mc08};
split(mc05_09) -> {fb05, mc09};
split(mc05_10) -> {fb05, mc10};
split(mc05_11) -> {fb05, mc11};
split(mc05_12) -> {fb05, mc12};
split(mc05_13) -> {fb05, mc13};
split(mc05_14) -> {fb05, mc14};
split(mc05_15) -> {fb05, mc15};
split(mc05_16) -> {fb05, mc16};
split(mc05_17) -> {fb05, mc17};
split(mc05_18) -> {fb05, mc18};
split(mc06_01) -> {fb06, mc01};
split(mc06_02) -> {fb06, mc02};
split(mc06_03) -> {fb06, mc03};
split(mc06_04) -> {fb06, mc04};
split(mc06_05) -> {fb06, mc05};
split(mc06_06) -> {fb06, mc06};
split(mc06_07) -> {fb06, mc07};
split(mc06_08) -> {fb06, mc08};
split(mc06_09) -> {fb06, mc09};
split(mc06_10) -> {fb06, mc10};
split(mc06_11) -> {fb06, mc11};
split(mc06_12) -> {fb06, mc12};
split(mc06_13) -> {fb06, mc13};
split(mc06_14) -> {fb06, mc14};
split(mc06_15) -> {fb06, mc15};
split(mc06_16) -> {fb06, mc16};
split(mc06_17) -> {fb06, mc17};
split(mc06_18) -> {fb06, mc18};
split(mc07_01) -> {fb07, mc01};
split(mc07_02) -> {fb07, mc02};
split(mc07_03) -> {fb07, mc03};
split(mc07_04) -> {fb07, mc04};
split(mc07_05) -> {fb07, mc05};
split(mc07_06) -> {fb07, mc06};
split(mc07_07) -> {fb07, mc07};
split(mc07_08) -> {fb07, mc08};
split(mc07_09) -> {fb07, mc09};
split(mc07_10) -> {fb07, mc10};
split(mc07_11) -> {fb07, mc11};
split(mc07_12) -> {fb07, mc12};
split(mc07_13) -> {fb07, mc13};
split(mc07_14) -> {fb07, mc14};
split(mc07_15) -> {fb07, mc15};
split(mc07_16) -> {fb07, mc16};
split(mc07_17) -> {fb07, mc17};
split(mc07_18) -> {fb07, mc18};
split(mc08_01) -> {fb08, mc01};
split(mc08_02) -> {fb08, mc02};
split(mc08_03) -> {fb08, mc03};
split(mc08_04) -> {fb08, mc04};
split(mc08_05) -> {fb08, mc05};
split(mc08_06) -> {fb08, mc06};
split(mc08_07) -> {fb08, mc07};
split(mc08_08) -> {fb08, mc08};
split(mc08_09) -> {fb08, mc09};
split(mc08_10) -> {fb08, mc10};
split(mc08_11) -> {fb08, mc11};
split(mc08_12) -> {fb08, mc12};
split(mc08_13) -> {fb08, mc13};
split(mc08_14) -> {fb08, mc14};
split(mc08_15) -> {fb08, mc15};
split(mc08_16) -> {fb08, mc16};
split(mc08_17) -> {fb08, mc17};
split(mc08_18) -> {fb08, mc18};
split(mc09_01) -> {fb09, mc01};
split(mc09_02) -> {fb09, mc02};
split(mc09_03) -> {fb09, mc03};
split(mc09_04) -> {fb09, mc04};
split(mc09_05) -> {fb09, mc05};
split(mc09_06) -> {fb09, mc06};
split(mc09_07) -> {fb09, mc07};
split(mc09_08) -> {fb09, mc08};
split(mc09_09) -> {fb09, mc09};
split(mc09_10) -> {fb09, mc10};
split(mc09_11) -> {fb09, mc11};
split(mc09_12) -> {fb09, mc12};
split(mc09_13) -> {fb09, mc13};
split(mc09_14) -> {fb09, mc14};
split(mc09_15) -> {fb09, mc15};
split(mc09_16) -> {fb09, mc16};
split(mc09_17) -> {fb09, mc17};
split(mc09_18) -> {fb09, mc18};
split(mc10_01) -> {fb10, mc01};
split(mc10_02) -> {fb10, mc02};
split(mc10_03) -> {fb10, mc03};
split(mc10_04) -> {fb10, mc04};
split(mc10_05) -> {fb10, mc05};
split(mc10_06) -> {fb10, mc06};
split(mc10_07) -> {fb10, mc07};
split(mc10_08) -> {fb10, mc08};
split(mc10_09) -> {fb10, mc09};
split(mc10_10) -> {fb10, mc10};
split(mc10_11) -> {fb10, mc11};
split(mc10_12) -> {fb10, mc12};
split(mc10_13) -> {fb10, mc13};
split(mc10_14) -> {fb10, mc14};
split(mc10_15) -> {fb10, mc15};
split(mc10_16) -> {fb10, mc16};
split(mc10_17) -> {fb10, mc17};
split(mc10_18) -> {fb10, mc18};
split(mc11_01) -> {fb11, mc01};
split(mc11_02) -> {fb11, mc02};
split(mc11_03) -> {fb11, mc03};
split(mc11_04) -> {fb11, mc04};
split(mc11_05) -> {fb11, mc05};
split(mc11_06) -> {fb11, mc06};
split(mc11_07) -> {fb11, mc07};
split(mc11_08) -> {fb11, mc08};
split(mc11_09) -> {fb11, mc09};
split(mc11_10) -> {fb11, mc10};
split(mc11_11) -> {fb11, mc11};
split(mc11_12) -> {fb11, mc12};
split(mc11_13) -> {fb11, mc13};
split(mc11_14) -> {fb11, mc14};
split(mc11_15) -> {fb11, mc15};
split(mc11_16) -> {fb11, mc16};
split(mc11_17) -> {fb11, mc17};
split(mc11_18) -> {fb11, mc18};
split(mc12_01) -> {fb12, mc01};
split(mc12_02) -> {fb12, mc02};
split(mc12_03) -> {fb12, mc03};
split(mc12_04) -> {fb12, mc04};
split(mc12_05) -> {fb12, mc05};
split(mc12_06) -> {fb12, mc06};
split(mc12_07) -> {fb12, mc07};
split(mc12_08) -> {fb12, mc08};
split(mc12_09) -> {fb12, mc09};
split(mc12_10) -> {fb12, mc10};
split(mc12_11) -> {fb12, mc11};
split(mc12_12) -> {fb12, mc12};
split(mc12_13) -> {fb12, mc13};
split(mc12_14) -> {fb12, mc14};
split(mc12_15) -> {fb12, mc15};
split(mc12_16) -> {fb12, mc16};
split(mc12_17) -> {fb12, mc17};
split(mc12_18) -> {fb12, mc18};
split(mc13_01) -> {fb13, mc01};
split(mc13_02) -> {fb13, mc02};
split(mc13_03) -> {fb13, mc03};
split(mc13_04) -> {fb13, mc04};
split(mc13_05) -> {fb13, mc05};
split(mc13_06) -> {fb13, mc06};
split(mc13_07) -> {fb13, mc07};
split(mc13_08) -> {fb13, mc08};
split(mc13_09) -> {fb13, mc09};
split(mc13_10) -> {fb13, mc10};
split(mc13_11) -> {fb13, mc11};
split(mc13_12) -> {fb13, mc12};
split(mc13_13) -> {fb13, mc13};
split(mc13_14) -> {fb13, mc14};
split(mc13_15) -> {fb13, mc15};
split(mc13_16) -> {fb13, mc16};
split(mc13_17) -> {fb13, mc17};
split(mc13_18) -> {fb13, mc18};
split(mc14_01) -> {fb14, mc01};
split(mc14_02) -> {fb14, mc02};
split(mc14_03) -> {fb14, mc03};
split(mc14_04) -> {fb14, mc04};
split(mc14_05) -> {fb14, mc05};
split(mc14_06) -> {fb14, mc06};
split(mc14_07) -> {fb14, mc07};
split(mc14_08) -> {fb14, mc08};
split(mc14_09) -> {fb14, mc09};
split(mc14_10) -> {fb14, mc10};
split(mc14_11) -> {fb14, mc11};
split(mc14_12) -> {fb14, mc12};
split(mc14_13) -> {fb14, mc13};
split(mc14_14) -> {fb14, mc14};
split(mc14_15) -> {fb14, mc15};
split(mc14_16) -> {fb14, mc16};
split(mc14_17) -> {fb14, mc17};
split(mc14_18) -> {fb14, mc18};
split(mc15_01) -> {fb15, mc01};
split(mc15_02) -> {fb15, mc02};
split(mc15_03) -> {fb15, mc03};
split(mc15_04) -> {fb15, mc04};
split(mc15_05) -> {fb15, mc05};
split(mc15_06) -> {fb15, mc06};
split(mc15_07) -> {fb15, mc07};
split(mc15_08) -> {fb15, mc08};
split(mc15_09) -> {fb15, mc09};
split(mc15_10) -> {fb15, mc10};
split(mc15_11) -> {fb15, mc11};
split(mc15_12) -> {fb15, mc12};
split(mc15_13) -> {fb15, mc13};
split(mc15_14) -> {fb15, mc14};
split(mc15_15) -> {fb15, mc15};
split(mc15_16) -> {fb15, mc16};
split(mc15_17) -> {fb15, mc17};
split(mc15_18) -> {fb15, mc18};
split(mc16_01) -> {fb16, mc01};
split(mc16_02) -> {fb16, mc02};
split(mc16_03) -> {fb16, mc03};
split(mc16_04) -> {fb16, mc04};
split(mc16_05) -> {fb16, mc05};
split(mc16_06) -> {fb16, mc06};
split(mc16_07) -> {fb16, mc07};
split(mc16_08) -> {fb16, mc08};
split(mc16_09) -> {fb16, mc09};
split(mc16_10) -> {fb16, mc10};
split(mc16_11) -> {fb16, mc11};
split(mc16_12) -> {fb16, mc12};
split(mc16_13) -> {fb16, mc13};
split(mc16_14) -> {fb16, mc14};
split(mc16_15) -> {fb16, mc15};
split(mc16_16) -> {fb16, mc16};
split(mc16_17) -> {fb16, mc17};
split(mc16_18) -> {fb16, mc18}.

%%====================================================================
%% join
%%====================================================================

-spec join(function_block(), macro_cell()) -> absolute().

join(fb01, mc01) -> mc01_01;
join(fb01, mc02) -> mc01_02;
join(fb01, mc03) -> mc01_03;
join(fb01, mc04) -> mc01_04;
join(fb01, mc05) -> mc01_05;
join(fb01, mc06) -> mc01_06;
join(fb01, mc07) -> mc01_07;
join(fb01, mc08) -> mc01_08;
join(fb01, mc09) -> mc01_09;
join(fb01, mc10) -> mc01_10;
join(fb01, mc11) -> mc01_11;
join(fb01, mc12) -> mc01_12;
join(fb01, mc13) -> mc01_13;
join(fb01, mc14) -> mc01_14;
join(fb01, mc15) -> mc01_15;
join(fb01, mc16) -> mc01_16;
join(fb01, mc17) -> mc01_17;
join(fb01, mc18) -> mc01_18;
join(fb02, mc01) -> mc02_01;
join(fb02, mc02) -> mc02_02;
join(fb02, mc03) -> mc02_03;
join(fb02, mc04) -> mc02_04;
join(fb02, mc05) -> mc02_05;
join(fb02, mc06) -> mc02_06;
join(fb02, mc07) -> mc02_07;
join(fb02, mc08) -> mc02_08;
join(fb02, mc09) -> mc02_09;
join(fb02, mc10) -> mc02_10;
join(fb02, mc11) -> mc02_11;
join(fb02, mc12) -> mc02_12;
join(fb02, mc13) -> mc02_13;
join(fb02, mc14) -> mc02_14;
join(fb02, mc15) -> mc02_15;
join(fb02, mc16) -> mc02_16;
join(fb02, mc17) -> mc02_17;
join(fb02, mc18) -> mc02_18;
join(fb03, mc01) -> mc03_01;
join(fb03, mc02) -> mc03_02;
join(fb03, mc03) -> mc03_03;
join(fb03, mc04) -> mc03_04;
join(fb03, mc05) -> mc03_05;
join(fb03, mc06) -> mc03_06;
join(fb03, mc07) -> mc03_07;
join(fb03, mc08) -> mc03_08;
join(fb03, mc09) -> mc03_09;
join(fb03, mc10) -> mc03_10;
join(fb03, mc11) -> mc03_11;
join(fb03, mc12) -> mc03_12;
join(fb03, mc13) -> mc03_13;
join(fb03, mc14) -> mc03_14;
join(fb03, mc15) -> mc03_15;
join(fb03, mc16) -> mc03_16;
join(fb03, mc17) -> mc03_17;
join(fb03, mc18) -> mc03_18;
join(fb04, mc01) -> mc04_01;
join(fb04, mc02) -> mc04_02;
join(fb04, mc03) -> mc04_03;
join(fb04, mc04) -> mc04_04;
join(fb04, mc05) -> mc04_05;
join(fb04, mc06) -> mc04_06;
join(fb04, mc07) -> mc04_07;
join(fb04, mc08) -> mc04_08;
join(fb04, mc09) -> mc04_09;
join(fb04, mc10) -> mc04_10;
join(fb04, mc11) -> mc04_11;
join(fb04, mc12) -> mc04_12;
join(fb04, mc13) -> mc04_13;
join(fb04, mc14) -> mc04_14;
join(fb04, mc15) -> mc04_15;
join(fb04, mc16) -> mc04_16;
join(fb04, mc17) -> mc04_17;
join(fb04, mc18) -> mc04_18;
join(fb05, mc01) -> mc05_01;
join(fb05, mc02) -> mc05_02;
join(fb05, mc03) -> mc05_03;
join(fb05, mc04) -> mc05_04;
join(fb05, mc05) -> mc05_05;
join(fb05, mc06) -> mc05_06;
join(fb05, mc07) -> mc05_07;
join(fb05, mc08) -> mc05_08;
join(fb05, mc09) -> mc05_09;
join(fb05, mc10) -> mc05_10;
join(fb05, mc11) -> mc05_11;
join(fb05, mc12) -> mc05_12;
join(fb05, mc13) -> mc05_13;
join(fb05, mc14) -> mc05_14;
join(fb05, mc15) -> mc05_15;
join(fb05, mc16) -> mc05_16;
join(fb05, mc17) -> mc05_17;
join(fb05, mc18) -> mc05_18;
join(fb06, mc01) -> mc06_01;
join(fb06, mc02) -> mc06_02;
join(fb06, mc03) -> mc06_03;
join(fb06, mc04) -> mc06_04;
join(fb06, mc05) -> mc06_05;
join(fb06, mc06) -> mc06_06;
join(fb06, mc07) -> mc06_07;
join(fb06, mc08) -> mc06_08;
join(fb06, mc09) -> mc06_09;
join(fb06, mc10) -> mc06_10;
join(fb06, mc11) -> mc06_11;
join(fb06, mc12) -> mc06_12;
join(fb06, mc13) -> mc06_13;
join(fb06, mc14) -> mc06_14;
join(fb06, mc15) -> mc06_15;
join(fb06, mc16) -> mc06_16;
join(fb06, mc17) -> mc06_17;
join(fb06, mc18) -> mc06_18;
join(fb07, mc01) -> mc07_01;
join(fb07, mc02) -> mc07_02;
join(fb07, mc03) -> mc07_03;
join(fb07, mc04) -> mc07_04;
join(fb07, mc05) -> mc07_05;
join(fb07, mc06) -> mc07_06;
join(fb07, mc07) -> mc07_07;
join(fb07, mc08) -> mc07_08;
join(fb07, mc09) -> mc07_09;
join(fb07, mc10) -> mc07_10;
join(fb07, mc11) -> mc07_11;
join(fb07, mc12) -> mc07_12;
join(fb07, mc13) -> mc07_13;
join(fb07, mc14) -> mc07_14;
join(fb07, mc15) -> mc07_15;
join(fb07, mc16) -> mc07_16;
join(fb07, mc17) -> mc07_17;
join(fb07, mc18) -> mc07_18;
join(fb08, mc01) -> mc08_01;
join(fb08, mc02) -> mc08_02;
join(fb08, mc03) -> mc08_03;
join(fb08, mc04) -> mc08_04;
join(fb08, mc05) -> mc08_05;
join(fb08, mc06) -> mc08_06;
join(fb08, mc07) -> mc08_07;
join(fb08, mc08) -> mc08_08;
join(fb08, mc09) -> mc08_09;
join(fb08, mc10) -> mc08_10;
join(fb08, mc11) -> mc08_11;
join(fb08, mc12) -> mc08_12;
join(fb08, mc13) -> mc08_13;
join(fb08, mc14) -> mc08_14;
join(fb08, mc15) -> mc08_15;
join(fb08, mc16) -> mc08_16;
join(fb08, mc17) -> mc08_17;
join(fb08, mc18) -> mc08_18;
join(fb09, mc01) -> mc09_01;
join(fb09, mc02) -> mc09_02;
join(fb09, mc03) -> mc09_03;
join(fb09, mc04) -> mc09_04;
join(fb09, mc05) -> mc09_05;
join(fb09, mc06) -> mc09_06;
join(fb09, mc07) -> mc09_07;
join(fb09, mc08) -> mc09_08;
join(fb09, mc09) -> mc09_09;
join(fb09, mc10) -> mc09_10;
join(fb09, mc11) -> mc09_11;
join(fb09, mc12) -> mc09_12;
join(fb09, mc13) -> mc09_13;
join(fb09, mc14) -> mc09_14;
join(fb09, mc15) -> mc09_15;
join(fb09, mc16) -> mc09_16;
join(fb09, mc17) -> mc09_17;
join(fb09, mc18) -> mc09_18;
join(fb10, mc01) -> mc10_01;
join(fb10, mc02) -> mc10_02;
join(fb10, mc03) -> mc10_03;
join(fb10, mc04) -> mc10_04;
join(fb10, mc05) -> mc10_05;
join(fb10, mc06) -> mc10_06;
join(fb10, mc07) -> mc10_07;
join(fb10, mc08) -> mc10_08;
join(fb10, mc09) -> mc10_09;
join(fb10, mc10) -> mc10_10;
join(fb10, mc11) -> mc10_11;
join(fb10, mc12) -> mc10_12;
join(fb10, mc13) -> mc10_13;
join(fb10, mc14) -> mc10_14;
join(fb10, mc15) -> mc10_15;
join(fb10, mc16) -> mc10_16;
join(fb10, mc17) -> mc10_17;
join(fb10, mc18) -> mc10_18;
join(fb11, mc01) -> mc11_01;
join(fb11, mc02) -> mc11_02;
join(fb11, mc03) -> mc11_03;
join(fb11, mc04) -> mc11_04;
join(fb11, mc05) -> mc11_05;
join(fb11, mc06) -> mc11_06;
join(fb11, mc07) -> mc11_07;
join(fb11, mc08) -> mc11_08;
join(fb11, mc09) -> mc11_09;
join(fb11, mc10) -> mc11_10;
join(fb11, mc11) -> mc11_11;
join(fb11, mc12) -> mc11_12;
join(fb11, mc13) -> mc11_13;
join(fb11, mc14) -> mc11_14;
join(fb11, mc15) -> mc11_15;
join(fb11, mc16) -> mc11_16;
join(fb11, mc17) -> mc11_17;
join(fb11, mc18) -> mc11_18;
join(fb12, mc01) -> mc12_01;
join(fb12, mc02) -> mc12_02;
join(fb12, mc03) -> mc12_03;
join(fb12, mc04) -> mc12_04;
join(fb12, mc05) -> mc12_05;
join(fb12, mc06) -> mc12_06;
join(fb12, mc07) -> mc12_07;
join(fb12, mc08) -> mc12_08;
join(fb12, mc09) -> mc12_09;
join(fb12, mc10) -> mc12_10;
join(fb12, mc11) -> mc12_11;
join(fb12, mc12) -> mc12_12;
join(fb12, mc13) -> mc12_13;
join(fb12, mc14) -> mc12_14;
join(fb12, mc15) -> mc12_15;
join(fb12, mc16) -> mc12_16;
join(fb12, mc17) -> mc12_17;
join(fb12, mc18) -> mc12_18;
join(fb13, mc01) -> mc13_01;
join(fb13, mc02) -> mc13_02;
join(fb13, mc03) -> mc13_03;
join(fb13, mc04) -> mc13_04;
join(fb13, mc05) -> mc13_05;
join(fb13, mc06) -> mc13_06;
join(fb13, mc07) -> mc13_07;
join(fb13, mc08) -> mc13_08;
join(fb13, mc09) -> mc13_09;
join(fb13, mc10) -> mc13_10;
join(fb13, mc11) -> mc13_11;
join(fb13, mc12) -> mc13_12;
join(fb13, mc13) -> mc13_13;
join(fb13, mc14) -> mc13_14;
join(fb13, mc15) -> mc13_15;
join(fb13, mc16) -> mc13_16;
join(fb13, mc17) -> mc13_17;
join(fb13, mc18) -> mc13_18;
join(fb14, mc01) -> mc14_01;
join(fb14, mc02) -> mc14_02;
join(fb14, mc03) -> mc14_03;
join(fb14, mc04) -> mc14_04;
join(fb14, mc05) -> mc14_05;
join(fb14, mc06) -> mc14_06;
join(fb14, mc07) -> mc14_07;
join(fb14, mc08) -> mc14_08;
join(fb14, mc09) -> mc14_09;
join(fb14, mc10) -> mc14_10;
join(fb14, mc11) -> mc14_11;
join(fb14, mc12) -> mc14_12;
join(fb14, mc13) -> mc14_13;
join(fb14, mc14) -> mc14_14;
join(fb14, mc15) -> mc14_15;
join(fb14, mc16) -> mc14_16;
join(fb14, mc17) -> mc14_17;
join(fb14, mc18) -> mc14_18;
join(fb15, mc01) -> mc15_01;
join(fb15, mc02) -> mc15_02;
join(fb15, mc03) -> mc15_03;
join(fb15, mc04) -> mc15_04;
join(fb15, mc05) -> mc15_05;
join(fb15, mc06) -> mc15_06;
join(fb15, mc07) -> mc15_07;
join(fb15, mc08) -> mc15_08;
join(fb15, mc09) -> mc15_09;
join(fb15, mc10) -> mc15_10;
join(fb15, mc11) -> mc15_11;
join(fb15, mc12) -> mc15_12;
join(fb15, mc13) -> mc15_13;
join(fb15, mc14) -> mc15_14;
join(fb15, mc15) -> mc15_15;
join(fb15, mc16) -> mc15_16;
join(fb15, mc17) -> mc15_17;
join(fb15, mc18) -> mc15_18;
join(fb16, mc01) -> mc16_01;
join(fb16, mc02) -> mc16_02;
join(fb16, mc03) -> mc16_03;
join(fb16, mc04) -> mc16_04;
join(fb16, mc05) -> mc16_05;
join(fb16, mc06) -> mc16_06;
join(fb16, mc07) -> mc16_07;
join(fb16, mc08) -> mc16_08;
join(fb16, mc09) -> mc16_09;
join(fb16, mc10) -> mc16_10;
join(fb16, mc11) -> mc16_11;
join(fb16, mc12) -> mc16_12;
join(fb16, mc13) -> mc16_13;
join(fb16, mc14) -> mc16_14;
join(fb16, mc15) -> mc16_15;
join(fb16, mc16) -> mc16_16;
join(fb16, mc17) -> mc16_17;
join(fb16, mc18) -> mc16_18.

