-module(density).

-export([list/0]).
-export([devices/1]).
-export([largest_device/1]).
-export([function_block_count/1]).
-export([function_blocks/1]).
-export([macro_cell_count/1]).
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
    [fb1, fb2];
function_blocks(xc9572xl) ->
    [fb1, fb2, fb3, fb4];
function_blocks(xc95144xl) ->
    [fb1, fb2, fb3, fb4, fb5, fb6, fb7, fb8];
function_blocks(xc95288xl) ->
    [fb1, fb2, fb3, fb4, fb5, fb6, fb7, fb8, fb9, fb10, fb11, fb12, fb13, fb14, fb15, fb16];
function_blocks(Device) ->
    function_blocks(device:density(Device)).

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

