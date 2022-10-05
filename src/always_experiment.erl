-module(always_experiment).

-export([run/0]).

%%  Check all cached experiments to confirm that some fuses are never off.

%%====================================================================
%% run
%%====================================================================

run() ->
    run(experiment:cache_iterate()).

%%--------------------------------------------------------------------

run(false) ->
    io:format("~n", []),
    ok;
run({Cache, Iterate}) ->
    io:format(".", []),
    With = experiment:cached_with(Cache),
    Fuses = experiment:cached_jed(Cache),
    run(With, Fuses),
    run(experiment:cache_iterate(Iterate)).

%%--------------------------------------------------------------------

run(With = #{device := Device}, Fuses) ->
    Density = device:density(Device),
    FBs = density:function_blocks(Density),
    run(With, Density, FBs, Fuses).

%%--------------------------------------------------------------------

run(_, _, [], _) ->
    ok;
run(With, Density, [FB | FBs], Fuses) ->
    Fuse = fuse_map:name(Density, {FB, always}),
    case lists:member(Fuse, Fuses) of
        true ->
            run(With, Density, FBs, Fuses);

        false ->
            throw({With, FB, Fuses})
    end.

