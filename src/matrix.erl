-module(matrix).

-export([all/1]).
-export([diff/1]).
-export([fuses/1]).
-export([print/1]).
-export([print_names/2]).

-type fuse() :: fuse:fuse().
-type name() :: atom() | binary() | string().
-type experiment() :: {name(), [fuse()]}.
-type matrix() :: {matrix, [fuse()], [{name(), [on | off]}]}.

%%====================================================================
%% all
%%====================================================================

-spec all([experiment()]) -> matrix().

all(Experiments) ->
    build(Experiments, all).

%%====================================================================
%% build
%%====================================================================

build(Experiments, All) when All =:= all orelse All =:= diff ->
    Fuses0 = [],
    Matches0 = [ [] || _ <- Experiments ],
    Results = [ Result || {_, Result} <- Experiments ],
    {Fuses, Matches} = build_diff(Results, Fuses0, Matches0, All),
    Matrix = lists:zipwith(fun build_zip/2, Experiments, Matches),
    {matrix, Fuses, Matrix}.

%%--------------------------------------------------------------------

build_diff([[] | _], Fuses, Matches, _) ->
    {lists:reverse(Fuses), [ lists:reverse(Match) || Match <- Matches ]};
build_diff(Results, Fuses, Matches, All) ->
    case build_min_max(Results) of
        {Fuse, Fuse} when All =:= diff ->
            build_diff(build_drop(Results, Fuse), Fuses, Matches, All);

        {Fuse, _} ->
            build_diff(
                build_drop(Results, Fuse),
                [Fuse | Fuses],
                build_match(Results, Fuse, Matches),
                All
             )
    end.

%%--------------------------------------------------------------------

build_min_max([[Fuse | _] | Results]) ->
    build_min_max(Results, Fuse, Fuse).

%%--------------------------------------------------------------------

build_min_max([], Min, Max) ->
    {Min, Max};
build_min_max([[Fuse | _] | Results], Min, Max) ->
    build_min_max(Results, min(Min, Fuse), max(Max, Fuse)).

%%--------------------------------------------------------------------

build_drop(Results, Drop) ->
    build_drop(Results, Drop, []).

%%--------------------------------------------------------------------

build_drop([], _, Tails) ->
    lists:reverse(Tails);
build_drop([[Drop | Tail] | Heads], Drop, Tails) ->
    build_drop(Heads, Drop, [Tail | Tails]);
build_drop([Tail | Heads], Drop, Tails) ->
    build_drop(Heads, Drop, [Tail | Tails]).

%%--------------------------------------------------------------------

build_match(Results, Fuse, Matches) ->
    build_match(Results, Fuse, Matches, []).

%%--------------------------------------------------------------------

build_match([], _, [], Outs) ->
    lists:reverse(Outs);
build_match([[Fuse | _] | Results], Fuse, [In | Ins], Outs) ->
    build_match(Results, Fuse, Ins, [[on | In] | Outs]);
build_match([_ | Results], Fuse, [In | Ins], Outs) ->
    build_match(Results, Fuse, Ins, [[off | In] | Outs]).

%%--------------------------------------------------------------------

build_zip({Name, _Result}, Matches) ->
    {Name, Matches}.

%%====================================================================
%% diff
%%====================================================================

-spec diff([experiment()]) -> matrix().

diff(Experiments) ->
    build(Experiments, diff).

%%====================================================================
%% fuses
%%====================================================================

-spec fuses([experiment()]) -> [fuse()].

fuses({matrix, Fuses, _}) ->
    Fuses.

%%====================================================================
%% print
%%====================================================================

-spec print(matrix()) -> ok.

print({matrix, Fuses, Matrix}) ->
    print_header(Fuses),
    print_rows(Matrix),
    ok.

%%--------------------------------------------------------------------

print_header(Fuses) ->
    case lists:max(Fuses) of
        Max when Max > 99999 ->
            print_header(Fuses, 100000);

        Max when Max > 9999 ->
            print_header(Fuses, 10000);

        Max when Max > 999 ->
            print_header(Fuses, 1000);

        Max when Max > 99 ->
            print_header(Fuses, 100);

        Max when Max > 9 ->
            print_header(Fuses, 10);

        _ ->
            print_header(Fuses, 1)
    end.

%%--------------------------------------------------------------------

print_header(Fuses, Depth = 1) ->
    print_header_row(Fuses, Depth);
print_header(Fuses, Depth) ->
    print_header_row(Fuses, Depth),
    print_header(Fuses, Depth div 10).

%%--------------------------------------------------------------------

print_header_row([], _) ->
    io:format("~n", []);
print_header_row([Fuse | Fuses], Depth) when Fuse < Depth ->
    io:format("  ", []),
    print_header_row(Fuses, Depth);
print_header_row([Fuse | Fuses], Depth) ->
    Digit = (Fuse div Depth) rem 10,
    io:format("~b ", [Digit]),
    print_header_row(Fuses, Depth).

%%--------------------------------------------------------------------

print_rows([]) ->
    ok;
print_rows([{Name, Fuses} | Rows]) ->
    print_fuses(Fuses),
    io:format(" ~s~n", [Name]),
    print_rows(Rows).

%%--------------------------------------------------------------------

print_fuses([]) ->
    ok;
print_fuses([on | Fuses]) ->
    io:format("*|", []),
    print_fuses(Fuses);
print_fuses([off | Fuses]) ->
    io:format(" |", []),
    print_fuses(Fuses).

%%====================================================================
%% print_names
%%====================================================================

-spec print_names(density:density() | device:device(), matrix()) -> ok.

print_names(DensityOrDevice, {matrix, Fuses, _}) ->
    Density = density:or_device(DensityOrDevice),
    io:format("~p~n", [[
        {Fuse, fuse_map:fuse(Density, Fuse)}
        ||
        Fuse <- Fuses
    ]]).

