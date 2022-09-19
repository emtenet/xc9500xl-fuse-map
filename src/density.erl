-module(density).

-export([list/0]).
-export([function_block_count/1]).
-export([function_blocks/1]).
-export([fuse_count/1]).

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
%% fuse_count
%%====================================================================

fuse_count(xc9536xl) -> 23328;
fuse_count(xc9572xl) -> 46656;
fuse_count(xc95144xl) -> 93312;
fuse_count(xc95288xl) -> 186624;
fuse_count(Device) ->
    fuse_count(device:density(Device)).

