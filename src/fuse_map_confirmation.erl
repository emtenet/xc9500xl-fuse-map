-module(fuse_map_confirmation).

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
    confirm(Density, maps:next(Iterator)).

%%--------------------------------------------------------------------

confirm(_, none) ->
    ok;
confirm(Density, {Fuse, Name, Iterator}) ->
    Expect = fuse_map:fuse(Density, Fuse),
    {fuse, Density, Fuse, is, Expect, expecting, Expect} =
    {fuse, Density, Fuse, is, Name, expecting, Expect},
    confirm(Density, maps:next(Iterator)).

