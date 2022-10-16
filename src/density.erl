-module(density).

-export([list/0]).
-export([devices/1]).
-export([largest_device/1]).
-export([function_block_count/1]).
-export([function_blocks/1]).
-export([input_choice_count/1]).
-export([input_choices/1]).
-export([macro_cell_count/1]).
-export([macro_cells/1]).
-export([fuse_count/1]).
-export([or_device/1]).

%%====================================================================
%% list
%%====================================================================

list() ->
    [
        xc9536xl,
        xc9572xl,
        xc95144xl,
        xc95288xl
    ].

%%====================================================================
%% devices
%%====================================================================

devices(xc9536xl) ->
    [
        xc9536xl_cs48,
        xc9536xl_vq44,
        xc9536xl_vq64
    ];
devices(xc9572xl) ->
    [
        xc9572xl_cs48,
        xc9572xl_tq100,
        xc9572xl_vq44,
        xc9572xl_vq64
    ];
devices(xc95144xl) ->
    [
        xc95144xl_cs144,
        xc95144xl_tq100,
        xc95144xl_tq144
    ];
devices(xc95288xl) ->
    [
        xc95288xl_bg256,
        xc95288xl_cs280,
        xc95288xl_fg256,
        xc95288xl_tq144
    ].

%%====================================================================
%% largest_device
%%====================================================================

largest_device(xc9536xl) -> xc9536xl_cs48; % 36 io pins
largest_device(xc9572xl) -> xc9572xl_tq100; % 72 io pins
largest_device(xc95144xl) -> xc95144xl_cs144; % 117 io pins
largest_device(xc95288xl) -> xc95288xl_cs280. % 192 io pins

%%====================================================================
%% function_block_count
%%====================================================================

function_block_count(xc9536xl) -> 2;
function_block_count(xc9572xl) -> 4;
function_block_count(xc95144xl) -> 8;
function_block_count(xc95288xl) -> 16;
function_block_count(Device) ->
    function_block_count(device:density(Device)).

%%====================================================================
%% function_blocks
%%====================================================================

function_blocks(xc9536xl) ->
    [fb01, fb02];
function_blocks(xc9572xl) ->
    [fb01, fb02, fb03, fb04];
function_blocks(xc95144xl) ->
    [fb01, fb02, fb03, fb04, fb05, fb06, fb07, fb08];
function_blocks(xc95288xl) ->
    [fb01, fb02, fb03, fb04, fb05, fb06, fb07, fb08, fb09, fb10, fb11, fb12, fb13, fb14, fb15, fb16];
function_blocks(Device) ->
    function_blocks(device:density(Device)).

%%====================================================================
%% input_choice_count
%%====================================================================

input_choice_count(xc9536xl) -> 2;
input_choice_count(xc9572xl) -> 4;
input_choice_count(xc95144xl) -> 8;
input_choice_count(xc95288xl) -> 11;
input_choice_count(Device) ->
    input_choice_count(device:density(Device)).

%%====================================================================
%% input_choices
%%====================================================================

input_choices(xc9536xl) ->
    [1, 2,
     16, 17];
input_choices(xc9572xl) ->
    [1, 2, 3, 4,
     16, 17, 18, 19];
input_choices(xc95144xl) ->
    [1, 2, 3, 4, 5, 6, 7, 8,
     16, 17, 18, 19, 20, 21, 22, 23];
input_choices(xc95288xl) ->
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
     16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31];
input_choices(Device) ->
    input_choices(device:density(Device)).

%%====================================================================
%% macro_cell_count
%%====================================================================

macro_cell_count(xc9536xl) -> 36;
macro_cell_count(xc9572xl) -> 72;
macro_cell_count(xc95144xl) -> 144;
macro_cell_count(xc95288xl) -> 288;
macro_cell_count(Device) ->
    macro_cell_count(device:density(Device)).

%%====================================================================
%% macro_cells
%%====================================================================

macro_cells(xc9536xl) ->
    [
        mc01_01,
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
        mc01_18,
        mc02_01,
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
        mc02_18
    ];
macro_cells(xc9572xl) ->
    [
        mc01_01,
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
        mc01_18,
        mc02_01,
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
        mc02_18,
        mc03_01,
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
        mc03_18,
        mc04_01,
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
        mc04_18
    ];
macro_cells(xc95144xl) ->
    [
        mc01_01,
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
        mc01_18,
        mc02_01,
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
        mc02_18,
        mc03_01,
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
        mc03_18,
        mc04_01,
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
        mc04_18,
        mc05_01,
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
        mc05_18,
        mc06_01,
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
        mc06_18,
        mc07_01,
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
        mc07_18,
        mc08_01,
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
        mc08_18
    ];
macro_cells(xc95288xl) ->
    [
        mc01_01,
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
        mc01_18,
        mc02_01,
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
        mc02_18,
        mc03_01,
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
        mc03_18,
        mc04_01,
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
        mc04_18,
        mc05_01,
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
        mc05_18,
        mc06_01,
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
        mc06_18,
        mc07_01,
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
        mc07_18,
        mc08_01,
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
        mc08_18,
        mc09_01,
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
        mc09_18,
        mc10_01,
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
        mc10_18,
        mc11_01,
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
        mc11_18,
        mc12_01,
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
        mc12_18,
        mc13_01,
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
        mc13_18,
        mc14_01,
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
        mc14_18,
        mc15_01,
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
        mc15_18,
        mc16_01,
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
        mc16_18
    ];
macro_cells(Device) ->
    macro_cells(device:density(Device)).

%%====================================================================
%% fuse_count
%%====================================================================

fuse_count(xc9536xl) -> 23328;
fuse_count(xc9572xl) -> 46656;
fuse_count(xc95144xl) -> 93312;
fuse_count(xc95288xl) -> 186624;
fuse_count(Device) ->
    fuse_count(device:density(Device)).

%%====================================================================
%% or_device
%%====================================================================

or_device(Density = xc9536xl) -> Density;
or_device(Density = xc9572xl) -> Density;
or_device(Density = xc95144xl) -> Density;
or_device(Density = xc95288xl) -> Density;
or_device(Device) ->
    device:density(Device).

