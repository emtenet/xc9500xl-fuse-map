-module(fuses).

%% operations
-export([diff/2]).
-export([intersect/2]).
-export([union/2]).
-export([subtract/2]).

%% database
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
%% intersect
%%====================================================================

intersect(As, Bs) ->
    intersect(As, Bs, []).

%%--------------------------------------------------------------------

intersect([], _, Is) ->
    lists:reverse(Is);
intersect(_, [], Is) ->
    lists:reverse(Is);
intersect([I | As], [I | Bs], Is) ->
    intersect(As, Bs, [I | Is]);
intersect([A | As], Bs = [B | _], Is) when A < B ->
    intersect(As, Bs, Is);
intersect(As, [_ | Bs], Is) ->
    intersect(As, Bs, Is).

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
%% update
%%====================================================================

-spec update(Density :: atom(), [{Fuse :: integer(), Name :: term()}]) -> ok.

update(_, []) ->
    ok;
update(DensityOrDevice, UpdateList) ->
    update_check(UpdateList),
    UpdateMap = maps:from_list(UpdateList),
    Density = density:or_device(DensityOrDevice),
    File = database_file(Density),
    Fuses = case read_file(File) of
        {ok, ExistingMap} ->
            maps:merge(ExistingMap, UpdateMap);

        false ->
            UpdateMap
    end,
    update_file(File, Fuses).

%%--------------------------------------------------------------------

update_check([]) ->
    ok;
update_check([{Fuse, _Name} | Fuses]) when is_integer(Fuse) ->
    update_check(Fuses).

%%--------------------------------------------------------------------

update_file(File, Fuses) ->
    Sorted = lists:sort(maps:to_list(Fuses)),
    Lines = [
        io_lib:format("~6..0b: ~p~n", [Fuse, Name])
        ||
        {Fuse, Name} <- Sorted
    ],
    ok = file:write_file(File, Lines).

%%====================================================================
%% read
%%====================================================================

read(DensityOrDevice) ->
    Density = density:or_device(DensityOrDevice),
    File = database_file(Density),
    case read_file(File) of
        {ok, Fuses} ->
            {fuses, Density, Fuses};

        false ->
            {fuses, Density, #{}}
    end.

%%--------------------------------------------------------------------

read_file(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            Lines = binary:split(Binary, <<"\n">>, [global]),
            read_lines(Lines, -1, #{});

        {error, enoent} ->
            false
    end.

%%--------------------------------------------------------------------

read_lines([], _, Fuses) ->
    {ok, Fuses};
read_lines([<<>>], _, Fuses) ->
    {ok, Fuses};
read_lines([Line | Lines], Previous, Fuses) ->
    case read_line(Line) of
        {Fuse, Name} when Fuse > Previous ->
            read_lines(Lines, Fuse, Fuses#{Fuse => Name})
    end.

%%--------------------------------------------------------------------

read_line(<<FuseBinary:6/binary, ": ", NameBinary/binary>>) ->
    Fuse = binary_to_integer(FuseBinary),
    NameString = binary_to_list(<<NameBinary/binary, ".">>),
    {ok, NameTokens, _} = erl_scan:string(NameString),
    {ok, Name} = erl_parse:parse_term(NameTokens),
    {Fuse, Name}.

%%====================================================================
%% name_if_known
%%====================================================================

name_if_known(Fuses, {fuses, _, FuseToName}) ->
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

database_file(Density) ->
    lists:flatten(io_lib:format("database/~s.fuses", [Density])).

