-module(fuse).

-export([and_array/3]).
-export([and_array/4]).
-export([fast_connect/4]).
-export([macro_cell/2]).

%%====================================================================
%% and_array
%%====================================================================

and_array(MacroCell, PT, Input) ->
    {FB, MC} = macro_cell:split(MacroCell),
    product_term:assert(PT),
    input:assert(Input),
    {FB, MC, PT, Input}.

%%--------------------------------------------------------------------

and_array(MacroCell, PT, Input, invert) ->
    {FB, MC} = macro_cell:split(MacroCell),
    product_term:assert(PT),
    input:assert(Input),
    {FB, MC, PT, Input, invert}.

%%====================================================================
%% fast_connect
%%====================================================================

fast_connect(FB, Input, Direction, From)
        when Direction =:= input orelse
             Direction =:= output ->
    function_block:assert(FB),
    input:assert(Input),
    {FromFB, FromMC} = macro_cell:split(From),
    {FB, Input, from, FromFB, Direction, FromMC}.

%%====================================================================
%% macro_cell
%%====================================================================

macro_cell(MacroCell, Fuse) when is_atom(Fuse) ->
    {FB, MC} = macro_cell:split(MacroCell),
    {FB, MC, Fuse}.

