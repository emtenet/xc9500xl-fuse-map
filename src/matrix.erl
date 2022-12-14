-module(matrix).

-export([all/1]).
-export([diff/1]).
-export([filter_by_name/3]).
-export([filter_by_name/4]).
-export([fuses/1]).
-export([names/2]).
-export([print/1]).
-export([print_names/2]).

-type fuse() :: fuse:fuse().
-type name() :: fuse_map:name().

-type experiment_name() :: atom() | binary() | string().
-type experiment() :: {experiment_name(), [fuse()]}.

-type matrix() :: {matrix, [fuse()], [{experiment_name(), [on | off]}]}.

-type density_or_device() :: density:density() | device:device().

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

build_diff(Results, Fuses, Matches, All) ->
    case build_min_max(Results) of
        finished ->
            {lists:reverse(Fuses), [
                lists:reverse(Match)
                ||
                Match <- Matches
            ]};

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

build_min_max([]) ->
    finished;
build_min_max([[] | Results]) ->
    build_min_max(Results);
build_min_max([[Fuse | _] | Results]) ->
    build_min_max(Results, Fuse, Fuse).

%%--------------------------------------------------------------------

build_min_max([], Min, Max) ->
    {Min, Max};
build_min_max([[] | Results], Min, Max) ->
    build_min_max(Results, Min, Max);
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
%% filter_by_name
%%====================================================================

-spec filter_by_name(Filter, density_or_device(), matrix())
        -> matrix()
    when Filter :: fun((name()) -> boolean()).

filter_by_name(Filter, DensityOrDevice, {matrix, Fuses, Matrix})
        when is_function(Filter, 1) ->
    Density = density:or_device(DensityOrDevice),
    Keep = [
        Filter(fuse_map:fuse(Density, Fuse))
        ||
        Fuse <- Fuses
    ],
    {matrix,
     filter_by_name_keep(Fuses, Keep, []),
     [
      {Name, filter_by_name_keep(Results, Keep, [])}
      ||
      {Name, Results} <- Matrix
     ]
    }.

%%--------------------------------------------------------------------

-spec filter_by_name(Filter, State, density_or_device(), matrix())
        -> matrix()
    when Filter :: fun((name(), State) -> boolean() | {boolean(), State}).

filter_by_name(Filter, State, DensityOrDevice, {matrix, Fuses, Matrix})
        when is_function(Filter, 2) ->
    Density = density:or_device(DensityOrDevice),
    Keep = filter_by_name_fold(Filter, State, Density, Fuses, []),
    {matrix,
     filter_by_name_keep(Fuses, Keep, []),
     [
      {Name, filter_by_name_keep(Results, Keep, [])}
      ||
      {Name, Results} <- Matrix
     ]
    }.

%%--------------------------------------------------------------------

filter_by_name_fold(_, _, _, [], Keep) ->
    lists:reverse(Keep);
filter_by_name_fold(Filter, State0, Density, [Fuse | Fuses], Keep) ->
    case Filter(fuse_map:fuse(Density, Fuse), State0) of
        true ->
            filter_by_name_fold(Filter, State0, Density, Fuses, [true | Keep]);

        false ->
            filter_by_name_fold(Filter, State0, Density, Fuses, [false | Keep]);

        {true, State1} ->
            filter_by_name_fold(Filter, State1, Density, Fuses, [true | Keep]);

        {false, State1} ->
            filter_by_name_fold(Filter, State1, Density, Fuses, [false | Keep])
    end.

%%--------------------------------------------------------------------

filter_by_name_keep([], [], Kept) ->
    lists:reverse(Kept);
filter_by_name_keep([Fuse | Fuses], [true | Keep], Kept) ->
    filter_by_name_keep(Fuses, Keep, [Fuse | Kept]);
filter_by_name_keep([_ | Fuses], [false | Keep], Kept) ->
    filter_by_name_keep(Fuses, Keep, Kept).

%%====================================================================
%% fuses
%%====================================================================

-spec fuses(matrix()) -> [fuse()].

fuses({matrix, Fuses, _}) ->
    Fuses.

%%====================================================================
%% names
%%====================================================================

-spec names(density_or_device(), matrix()) -> [name()].

names(DensityOrDevice, {matrix, Fuses, _}) ->
    Density = density:or_device(DensityOrDevice),
    fuse_map:fuses(Density, Fuses).

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

-spec print_names(density_or_device(), matrix()) -> ok.

print_names(DensityOrDevice, {matrix, Fuses, _}) ->
    Density = density:or_device(DensityOrDevice),
    io:format("~p~n", [[
        {Fuse, fuse_map:fuse(Density, Fuse)}
        ||
        Fuse <- Fuses
    ]]).

