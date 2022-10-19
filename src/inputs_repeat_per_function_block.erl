-module(inputs_repeat_per_function_block).

-export([run/0]).

%%  Confirm that:
%%  
%%      The mapping
%%       - from input (1..54) choice (1..c, 16..15+c)
%%       - to macro cell input/ooutput
%%      is the same for ALL function blocks

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format("inputs-repeat-per-funcion-block ~p~n", [Density]),
    repeat_per_function_block(Density),
    io:format("OK~n", []),
    ok.

%%--------------------------------------------------------------------

repeat_per_function_block(Density) ->
    inputs:common(inputs:read(Density)).

