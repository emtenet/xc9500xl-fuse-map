-module(fuses).

%% operations
-export([diff/2]).
-export([union/2]).
-export([subtract/2]).

%% visual aids
-export([print/1]).
-export([matrix/1]).

%% database
-export([report/0]).
-export([update/2]).
-export([read/1]).
-export([name_if_known/2]).

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
%% union
%%====================================================================

union(Left, Right) ->
    lists:umerge(Left, Right).

%%====================================================================
%% subtract
%%====================================================================

subtract([], _) ->
    [];
subtract(Fuses, []) ->
    Fuses;
subtract(Fs, [S | Ss]) ->
    subtract(Fs, S, Ss, []).

%%--------------------------------------------------------------------

subtract([], _, _, Ks) ->
    lists:reverse(Ks);
subtract(Fs = [F | _], S, [], Ks) when S < F ->
    lists:reverse(Ks, Fs);
subtract(Fs = [F | _], S, [Sh | Ss], Ks) when S < F ->
    subtract(Fs, Sh, Ss, Ks);
subtract([F | Fs], S, [], Ks) when S =:= F ->
    lists:reverse(Ks, Fs);
subtract([F | Fs], S, [Sh | Ss], Ks) when S =:= F ->
    subtract(Fs, Sh, Ss, Ks);
subtract([F | Fs], S, Ss, Ks) ->
    subtract(Fs, S, Ss, [F | Ks]).

%%====================================================================
%% print
%%====================================================================

print({matrix, Fuses, Matrix}) ->
    matrix_header(Fuses),
    matrix_rows(Matrix),
    ok.

%%====================================================================
%% matrix
%%====================================================================

-type fuse() :: pos_integer().
-type name() :: atom() | binary() | string().
-type experiment() :: {name(), [fuse()]}.
-spec matrix([experiment()]) -> {matrix, [fuse()], [{name(), [on | off]}]}.

matrix(Experiments) ->
    Fuses0 = [],
    Matches0 = [ [] || _ <- Experiments ],
    Results = [ Result || {_, Result} <- Experiments ],
    {Fuses, Matches} = matrix_diff(Results, Fuses0, Matches0),
    Matrix = lists:zipwith(fun matrix_zip/2, Experiments, Matches),
    {matrix, Fuses, Matrix}.

%%--------------------------------------------------------------------

matrix_diff([[] | _], Fuses, Matches) ->
    {lists:reverse(Fuses), [ lists:reverse(Match) || Match <- Matches ]};
matrix_diff(Results, Fuses, Matches) ->
    case matrix_min_max(Results) of
        {Fuse, Fuse} ->
            matrix_diff(matrix_drop(Results, Fuse), Fuses, Matches);

        {Fuse, _} ->
            matrix_diff(
                matrix_drop(Results, Fuse),
                [Fuse | Fuses],
                matrix_match(Results, Fuse, Matches)
             )
    end.

%%--------------------------------------------------------------------

matrix_min_max([[Fuse | _] | Results]) ->
    matrix_min_max(Results, Fuse, Fuse).

%%--------------------------------------------------------------------

matrix_min_max([], Min, Max) ->
    {Min, Max};
matrix_min_max([[Fuse | _] | Results], Min, Max) ->
    matrix_min_max(Results, min(Min, Fuse), max(Max, Fuse)).

%%--------------------------------------------------------------------

matrix_drop(Results, Drop) ->
    matrix_drop(Results, Drop, []).

%%--------------------------------------------------------------------

matrix_drop([], _, Tails) ->
    lists:reverse(Tails);
matrix_drop([[Drop | Tail] | Heads], Drop, Tails) ->
    matrix_drop(Heads, Drop, [Tail | Tails]);
matrix_drop([Tail | Heads], Drop, Tails) ->
    matrix_drop(Heads, Drop, [Tail | Tails]).

%%--------------------------------------------------------------------

matrix_match(Results, Fuse, Matches) ->
    matrix_match(Results, Fuse, Matches, []).

%%--------------------------------------------------------------------

matrix_match([], _, [], Outs) ->
    lists:reverse(Outs);
matrix_match([[Fuse | _] | Results], Fuse, [In | Ins], Outs) ->
    matrix_match(Results, Fuse, Ins, [[on | In] | Outs]);
matrix_match([_ | Results], Fuse, [In | Ins], Outs) ->
    matrix_match(Results, Fuse, Ins, [[off | In] | Outs]).

%%--------------------------------------------------------------------

matrix_zip({Name, _Result}, Matches) ->
    {Name, Matches}.

%%--------------------------------------------------------------------

matrix_header(Fuses) ->
    case lists:max(Fuses) of
        Max when Max > 99999 ->
            matrix_header(Fuses, 100000);

        Max when Max > 9999 ->
            matrix_header(Fuses, 10000);

        Max when Max > 999 ->
            matrix_header(Fuses, 1000);

        Max when Max > 99 ->
            matrix_header(Fuses, 100);

        Max when Max > 9 ->
            matrix_header(Fuses, 10);

        _ ->
            matrix_header(Fuses, 1)
    end.

%%--------------------------------------------------------------------

matrix_header(Fuses, Depth = 1) ->
    matrix_header_row(Fuses, Depth);
matrix_header(Fuses, Depth) ->
    matrix_header_row(Fuses, Depth),
    matrix_header(Fuses, Depth div 10).

%%--------------------------------------------------------------------

matrix_header_row([], _) ->
    io:format("~n", []);
matrix_header_row([Fuse | Fuses], Depth) when Fuse < Depth ->
    io:format("  ", []),
    matrix_header_row(Fuses, Depth);
matrix_header_row([Fuse | Fuses], Depth) ->
    Digit = (Fuse div Depth) rem 10,
    io:format("~b ", [Digit]),
    matrix_header_row(Fuses, Depth).

%%--------------------------------------------------------------------

matrix_rows([]) ->
    ok;
matrix_rows([{Name, Fuses} | Rows]) ->
    matrix_fuses(Fuses),
    io:format(" ~s~n", [Name]),
    matrix_rows(Rows).

%%--------------------------------------------------------------------

matrix_fuses([]) ->
    ok;
matrix_fuses([on | Fuses]) ->
    io:format("*|", []),
    matrix_fuses(Fuses);
matrix_fuses([off | Fuses]) ->
    io:format(" |", []),
    matrix_fuses(Fuses).

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
%% update
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
%% read
%%====================================================================

read(DensityOrDevice) ->
    Density = density:or_device(DensityOrDevice),
    File = data_file(Density),
    case file:consult(File) of
        {ok, [Names, Numbers]} ->
            {fuses, Density, Names, Numbers};

        {error, enoent} ->
            {fuses, Density, #{}, #{}}
    end.

%%====================================================================
%% name_if_known
%%====================================================================

name_if_known(Fuses, {fuses, _, _, FuseToName}) ->
    lists:map(fun (Fuse) ->
        case FuseToName of
            #{Fuse := Name} ->
                Name;

            _ ->
                Fuse
        end
    end, Fuses).

%%====================================================================
%% helpers
%%====================================================================

data_file(Density) ->
    lists:flatten(io_lib:format("fuses/~s.data", [Density])).

%%--------------------------------------------------------------------

report_file(Density) ->
    lists:flatten(io_lib:format("fuses/~s.txt", [Density])).

