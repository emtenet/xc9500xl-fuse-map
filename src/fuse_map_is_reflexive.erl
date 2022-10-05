-module(fuse_map_is_reflexive).

-export([run/0]).

%%  Confirm that:
%%      Fuse = fuse_map:name(fuse_map:fuse(Fuse))
%%  for all fuses numbers.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format("fuse-map-is-reflexive ~p~n", [Density]),
    FuseCount = density:fuse_count(Density),
    fuses(Density, 0, FuseCount, FuseCount div 108),
    io:format("~n", []).

%%--------------------------------------------------------------------

fuses(_, Stop, Stop, _) ->
    ok;
fuses(Density, Fuse, Stop, Dot) when Fuse rem Dot =:= 0 ->
    io:format(".", []),
    fuse(Density, Fuse),
    fuses(Density, Fuse + 1, Stop, Dot);
fuses(Density, Fuse, Stop, Dot) ->
    fuse(Density, Fuse),
    fuses(Density, Fuse + 1, Stop, Dot).

%%--------------------------------------------------------------------

fuse(Density, Fuse) ->
    Name = fuse_map:fuse(Density, Fuse),
    case fuse_map:name(Density, Name) of
        Fuse ->
            ok;

        Diff ->
            throw({Fuse, Name, Diff})
    end.

