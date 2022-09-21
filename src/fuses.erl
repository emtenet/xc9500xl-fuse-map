-module(fuses).

-export([diff/2]).
-export([report/0]).
-export([update/2]).

%%====================================================================
%% diff
%%====================================================================

diff(From, Thru) ->
    diff(From, Thru, [], []).

%%--------------------------------------------------------------------

diff([], [], Add, Del) ->
    {lists:reverse(Add), lists:reverse(Del)};
diff([], After, Add, Del) ->
    {lists:reverse(Add, After), lists:reverse(Del)};
diff(Before, [], Add, Del) ->
    {lists:reverse(Add), lists:reverse(Del, Before)};
diff([B | Before], [A | After], Add, Del) when B =:= A ->
    diff(Before, After, Add, Del);
diff([B | Before], After = [A | _], Add, Del) when B < A ->
    diff(Before, After, Add, [B | Del]);
diff(Before, [A | After], Add, Del) ->
    diff(Before, After, [A | Add], Del).

%%====================================================================
%% report
%%====================================================================

report() ->
    lists:foreach(fun report/1, density:list()).

%%--------------------------------------------------------------------

report(Density) ->
    Max = density:fuse_count(Density),
    {ok, [_, Numbers0]} = file:consult(data_file(Density)),
    Numbers = lists:sort(maps:to_list(Numbers0)),
    Data = report(0, Numbers, [], Max),
    ok = file:write_file(report_file(Density), Data).

%%--------------------------------------------------------------------

report(Fuse, [], Lines, Max) when Fuse =:= Max ->
    lists:reverse(Lines);
report(Fuse, [], Lines, Max) when Fuse < Max ->
    Line = io_lib:format("~6..0b:~n", [Fuse]),
    report(Fuse + 1, [], [Line | Lines], Max);
report(Fuse, Fuses = [{Next, _} | _], Lines, Max) when Fuse < Next ->
    Line = io_lib:format("~6..0b:~n", [Fuse]),
    report(Fuse + 1, Fuses, [Line | Lines], Max);
report(Fuse, [{Fuse, Name} | Fuses], Lines, Max) ->
    Line = io_lib:format("~6..0b: ~p~n", [Fuse, Name]),
    report(Fuse + 1, Fuses, [Line | Lines], Max).

%%====================================================================
%% list
%%====================================================================

update(_, []) ->
    ok;
update(DensityOrDevice, AddNames) ->
    Density = density:or_device(DensityOrDevice),
    File = data_file(Density),
    case file:consult(File) of
        {ok, [Names, Numbers]} ->
            update(File, Names, Numbers, AddNames);

        {error, enoent} ->
            update(File, #{}, #{}, AddNames)
    end.

%%--------------------------------------------------------------------

update(File, Names0, Numbers0, AddNames) ->
    AddNumbers = [{Number, Name} || {Name, Number} <- AddNames],
    Names = maps:merge(Names0, maps:from_list(AddNames)),
    Numbers = maps:merge(Numbers0, maps:from_list(AddNumbers)),
    Data = io_lib:format("~p.~n~p.~n", [Names, Numbers]),
    ok = file:write_file(File, Data).

%%====================================================================
%% helpers
%%====================================================================

data_file(Density) ->
    lists:flatten(io_lib:format("fuses/~s.data", [Density])).

%%--------------------------------------------------------------------

report_file(Density) ->
    lists:flatten(io_lib:format("fuses/~s.txt", [Density])).

