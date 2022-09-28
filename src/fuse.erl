-module(fuse).

-export([and_array/3]).
-export([and_array/4]).
-export([fast_connect/4]).
-export([macro_cell/2]).

-type user() ::
    user00 | user01 | user02 | user03 | user04 | user05 | user06 | user07 |
    user08 | user09 | user10 | user11 | user12 | user13 | user14 | user15 |
    user16 | user17 | user18 | user19 | user20 | user21 | user22 | user23 |
    user24 | user25 | user26 | user27 | user28 | user29 | user30 | user13.
-export_type([user/0]).

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

