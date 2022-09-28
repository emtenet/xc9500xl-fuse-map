-module(fuse_map_validate).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    {fuses, Density, Fuses} = fuses:read(Density),
    Iterator = maps:iterator(Fuses),
    validate(Density, maps:next(Iterator)).

%%--------------------------------------------------------------------

validate(_, none) ->
    ok;
validate(Density, {Fuse, Name, Iterator}) ->
    Expect = fuse_map:fuse(Density, Fuse),
    {fuse, Density, Fuse, is, Expect, expecting, Expect} =
    {fuse, Density, Fuse, is, Name, expecting, Expect},
    validate(Density, maps:next(Iterator)).

