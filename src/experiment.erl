-module(experiment).

-export([compile/1]).
-export([run/1]).
-export([cache/1]).
-export([pins/0]).
-export([fuse_count/0]).
-export([jed/0]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% compile
%%====================================================================

-type oe() :: atom() | {atom(), low}.
-type logic() ::
    {Net :: atom(), Loc :: atom()} |
    {Net :: atom(), Loc :: atom(), Logic :: binary()} |
    {Net :: atom(), Loc :: atom(), Logic :: binary(), OE :: oe()}.
-spec compile([logic()]) -> {binary(), binary()}.

compile(Pins) ->
    UCF = iolist_to_binary([ compile_ucf(Pin) || Pin <- Pins ]),
    VHDL = iolist_to_binary([<<
        "library IEEE;\n"
        "use IEEE.STD_LOGIC_1164.ALL;\n"
        "\n"
        "entity experiment is\n"
        "  port (\n">>,
        lists:join(<<";\n">>, [ compile_port(Pin) || Pin <- Pins ]), <<"\n"
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is begin\n">>,
        [ compile_vhdl(Pin) || Pin <- Pins ], <<
        "end behavioral;\n"
    >>]),
    {UCF, VHDL}.

%%--------------------------------------------------------------------

compile_ucf({Net, Loc}) ->
    compile_ucf(atom_to_binary(Net, latin1), macro_cell:name(Loc));
compile_ucf({Net, Loc, _}) ->
    compile_ucf(atom_to_binary(Net, latin1), macro_cell:name(Loc));
compile_ucf({Net, Loc, _, _}) ->
    compile_ucf(atom_to_binary(Net, latin1), macro_cell:name(Loc)).

%%--------------------------------------------------------------------

compile_ucf(Net, Loc) ->
    <<"NET \"", Net/binary, "\" LOC = \"", Loc/binary, "\";\n">>.

%%--------------------------------------------------------------------

compile_port({Net, _}) ->
    compile_port(atom_to_binary(Net, latin1), <<"in">>);
compile_port({Net, _, _}) ->
    compile_port(atom_to_binary(Net, latin1), <<"out">>);
compile_port({Net, _, _, _}) ->
    compile_port(atom_to_binary(Net, latin1), <<"out">>).

%%--------------------------------------------------------------------

compile_port(Net, Dir) ->
    <<"    ", Net/binary, " : ", Dir/binary, "  STD_LOGIC">>.

%%--------------------------------------------------------------------

compile_vhdl({_, _}) ->
    <<>>;
compile_vhdl({Net_, _, Logic}) ->
    Net = atom_to_binary(Net_, latin1),
    <<"  ", Net/binary, " <= ", Logic/binary, ";\n">>;
compile_vhdl({Net_, _, Logic, {OE_, low}}) ->
    Net = atom_to_binary(Net_, latin1),
    OE = atom_to_binary(OE_, latin1),
    <<"  ", Net/binary, " <= (", Logic/binary, ") when (", OE/binary, " = '0') else 'Z';\n">>;
compile_vhdl({Net_, _, Logic, OE_}) ->
    Net = atom_to_binary(Net_, latin1),
    OE = atom_to_binary(OE_, latin1),
    <<"  ", Net/binary, " <= (", Logic/binary, ") when (", OE/binary, " = '1') else 'Z';\n">>.

%%====================================================================
%% internal
%%====================================================================

internal(With = #{device := Device}) ->
    Dir = dir(),
    make_dir(Dir),
    clear_dir(Dir),
    make_tmp(Dir),
    make_prj(Dir),
    make_xst(Dir),
    make_ucf(Dir, With),
    make_vhdl(Dir, With),
    ok = exec(Dir, "xst", [
        "-intstyle", "ise",
        "-ifn", "experiment.xst"
    ]),
    ok = exec(Dir, "ngdbuild", [
        "experiment.ngc",
        "experiment.ngd",
        "-intstyle", "ise",
        "-dd", "_ngo",
        "-uc", "experiment.ucf",
        "-p", device:name(Device)
    ]),
    ok = exec(Dir, "cpldfit", [
        "experiment.ngd",
        "-intstyle", "ise",
        "-p", device:name(Device),
        "-ofmt", "vhdl",
        "-loc", "on",
        "-slew", with_slew(With),
        "-init", with_init(With),
        "-inputs", "54",
        "-keepio",
        "-pterms", "50",
        "-unused", with_unused(With),
        "-power", with_power(With),
        "-terminate", with_terminate(With)
    ]),
    ok = exec(Dir, "hprep6", [
        "-s", "IEEE1149",
        "-n", maps:get(usercode, With),
        "-i", "experiment.vm6"
    ]),
    ok.

%%--------------------------------------------------------------------

with_defaults(With) ->
    maps:merge(#{
        init => low,
        power => std,
        slew => fast,
        terminate => keeper,
        unused => float,
        usercode => <<"@@@@">>
    }, With).

%%--------------------------------------------------------------------

with_init(#{init := high}) -> "high";
with_init(#{init := low}) -> "low".

%%--------------------------------------------------------------------

with_power(#{power := auto}) -> "auto";
with_power(#{power := low}) -> "low";
with_power(#{power := std}) -> "std".

%%--------------------------------------------------------------------

with_slew(#{slew := auto}) -> "auto";
with_slew(#{slew := fast}) -> "fast";
with_slew(#{slew := slow}) -> "slow".

%%--------------------------------------------------------------------

with_terminate(#{terminate := float}) -> "float";
with_terminate(#{terminate := keeper}) -> "keeper".

%%--------------------------------------------------------------------

with_unused(#{unused := float}) -> "float";
with_unused(#{unused := ground}) -> "ground".

%%====================================================================
%% run
%%====================================================================

run(With) ->
    internal(with_defaults(With)).

%%====================================================================
%% cache
%%====================================================================

cache(With0) ->
    With = with_defaults(With0),
    Query = cache_query(With),
    Dir = cache_dir(Query),
    case cache_read_query(Dir) of
        {ok, Query} ->
            cache_read_jed(Dir);

        {error, enoent} ->
            internal(With),
            cache_copy(Dir, "experiment.jed"),
            cache_copy(Dir, "experiment_pad.csv"),
            cache_copy(Dir, "experiment.rpt"),
            cache_copy(Dir, "experiment.vm6"),
            JED = jed(),
            cache_write_jed(Dir, JED),
            cache_write_query(Dir, Query),
            JED
    end.

%%--------------------------------------------------------------------

cache_query(With) ->
    iolist_to_binary(io_lib:format("~p.", [With])).

%%--------------------------------------------------------------------

cache_dir(Query) ->
    Hash = crypto:hash(sha256, Query),
    Base64 = base64url:encode(Hash),
    cache_make_dir("cache"),
    cache_make_dir(filename:join(["cache", "db"])),
    cache_make_dir(filename:join(["cache", "db", Base64])).

%%--------------------------------------------------------------------

cache_make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            Dir;

        {error, eexist} ->
            Dir
    end.

%%--------------------------------------------------------------------

cache_read_query(Dir) ->
    File = filename:join(Dir, "with"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_read_jed(Dir) ->
    File = filename:join(Dir, "jed"),
    {ok, [JED]} = file:consult(File),
    JED.

%%--------------------------------------------------------------------

cache_copy(Dir, Name) ->
    Source = filename:join(dir(), Name),
    Destination = filename:join(Dir, Name),
    {ok, _} = file:copy(Source, Destination),
    ok.

%%--------------------------------------------------------------------

cache_write_jed(Dir, JED) ->
    File = filename:join(Dir, "jed"),
    Data = io_lib:format("~p.", [JED]),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

cache_write_query(Dir, Query) ->
    File = filename:join(Dir, "with"),
    ok = file:write_file(File, Query).

%%====================================================================
%% pins
%%====================================================================

pins() ->
    File = filename:join(dir(), "experiment_pad.csv"),
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    pins(Lines).

%%--------------------------------------------------------------------

pins([<<"Pin Number,Signal Name,Pin Usage,Pin Name,Direction,", _/binary>> | Lines]) ->
    pins(Lines, []);
pins([_ | Lines]) ->
    pins(Lines).

%%--------------------------------------------------------------------

pins([<<>> | _], Pads) ->
    lists:reverse(Pads);
pins([Line | Lines], Pads) ->
    pins(Lines, [pin(Line) | Pads]).

%%--------------------------------------------------------------------

pin(Line) ->
    Fields = binary:split(Line, <<",">>, [global]),
    [Pin, Signal, Usage, Name, Direction | _] = Fields,
    #{
        pin => Pin,
        signal => Signal,
        usage => Usage,
        name => Name,
        direction => Direction
    }.

%%====================================================================
%% fuse_count
%%====================================================================

fuse_count() ->
    File = filename:join(dir(), "experiment.jed"),
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    fuse_count(Lines, 0).

%%--------------------------------------------------------------------

fuse_count([], Max) ->
    Max;
fuse_count([<<"L", Fuse_:7/binary, " ", Line/binary>> | Lines], _) ->
    Fuse = binary_to_integer(Fuse_),
    fuse_count(Fuse, Line, Lines);
fuse_count([_ | Lines], Max) ->
    fuse_count(Lines, Max).

%%--------------------------------------------------------------------

fuse_count(Fuse, <<"*">>, Lines) ->
    fuse_count(Lines, Fuse);
fuse_count(Fuse, <<" ", Line/binary>>, Lines) ->
    fuse_count(Fuse, Line, Lines);
fuse_count(Fuse, <<"0", Line/binary>>, Lines) ->
    fuse_count(Fuse + 1, Line, Lines);
fuse_count(Fuse, <<"1", Line/binary>>, Lines) ->
    fuse_count(Fuse + 1, Line, Lines).

%%====================================================================
%% jed
%%====================================================================

jed() ->
    File = filename:join(dir(), "experiment.jed"),
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    jed(Lines, []).

%%--------------------------------------------------------------------

jed([], Fuses) ->
    lists:reverse(Fuses);
jed([<<"L", Fuse_:7/binary, " ", Line/binary>> | Lines], Fuses) ->
    Fuse = binary_to_integer(Fuse_),
    jed(Fuse, Line, Lines, Fuses);
jed([_ | Lines], Fuses) ->
    jed(Lines, Fuses).

%%--------------------------------------------------------------------

jed(_, <<"*">>, Lines, Fuses) ->
    jed(Lines, Fuses);
jed(Fuse, <<" ", Line/binary>>, Lines, Fuses) ->
    jed(Fuse, Line, Lines, Fuses);
jed(Fuse, <<"0", Line/binary>>, Lines, Fuses) ->
    jed(Fuse + 1, Line, Lines, Fuses);
jed(Fuse, <<"1", Line/binary>>, Lines, Fuses) ->
    jed(Fuse + 1, Line, Lines, [Fuse | Fuses]).

%%====================================================================
%% helpers
%%====================================================================

clear_dir(Dir) ->
    {ok, Names} = file:list_dir_all(Dir),
    lists:foreach(fun (Name) ->
        clear_file(filename:join(Dir, Name))
    end, Names).

%%--------------------------------------------------------------------

clear_file(File) ->
    case file:read_link_info(File) of
        {ok, #file_info{type = directory}} ->
            clear_dir(File),
            ok = file:del_dir(File);

        {ok, _} ->
            ok = file:delete(File)
    end.

%%--------------------------------------------------------------------

dir() ->
    %{ok, Dir} = file:get_cwd(),
    %filename:join(Dir, "experiment").
    "experiment".

%%--------------------------------------------------------------------

exec(Dir, Arg0, Args) ->
    Path = "/home/Xilinx/12.4/ISE_DS/ISE/bin/lin64",
    Exec = {spawn_executable, filename:join(Path, Arg0)},
    Opts = [
        {args, Args},
        {cd, Dir},
        stream,
        exit_status,
        use_stdio,
        binary,
        eof
    ],
    Port = erlang:open_port(Exec, Opts),
    Result = exec(Port, []),
    erlang:port_close(Port),
    case Result of
        {0, _} ->
            ok;

        {Exit, Out} ->
            io:format("[[[[~n~s~n]]]]~nEXIT: ~p~n", [Out, Exit]),
            error
    end.

%%--------------------------------------------------------------------

exec(Port, Out) ->
    receive
        {Port, eof} ->
            receive
                {Port, {exit_status, Exit}} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {exit_status, Exit}} ->
            receive
                {Port, eof} ->
                    {Exit, lists:reverse(Out)}
            end;

        {Port, {data, Data}} ->
            exec(Port, [Data | Out])
    end.

%%--------------------------------------------------------------------

make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%%--------------------------------------------------------------------

make_file(Dir, Name, Data) ->
    File = filename:join(Dir, Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

make_prj(Dir) ->
    make_file(Dir, "experiment.prj", <<
        "vhdl work experiment.vhd\n"
    >>).

%%--------------------------------------------------------------------

make_tmp(Dir) ->
    ok = file:make_dir(filename:join(Dir, "tmp")).

%%--------------------------------------------------------------------

make_ucf(Dir, #{ucf := UCF}) ->
    make_file(Dir, "experiment.ucf", UCF).

%%--------------------------------------------------------------------

make_vhdl(Dir, #{vhdl := VHDL}) ->
    make_file(Dir, "experiment.vhd", VHDL).

%%--------------------------------------------------------------------

make_xst(Dir) ->
    make_file(Dir, "experiment.xst", <<
        "set -tmpdir \"tmp\"\n"
        "set -xsthdpdir \"xst\"\n"
        "run\n"
        "-ifn experiment.prj\n"
        "-ofn experiment\n"
        "-ifmt mixed\n"
        "-ofmt NGC\n"
        "-p xc9500xl\n"
        "-top experiment\n"
        "-opt_mode Speed\n"
        "-opt_level 1\n"
        "-iuc NO\n"
        "-keep_hierarchy Yes\n"
        "-netlist_hierarchy As_Optimized\n"
        "-rtlview Yes\n"
        "-hierarchy_separator /\n"
        "-bus_delimiter <>\n"
        "-case Maintain\n"
        "-verilog2001 YES\n"
        "-fsm_extract YES -fsm_encoding Auto\n"
        "-safe_implementation No\n"
        "-mux_extract Yes\n"
        "-resource_sharing YES\n"
        "-iobuf YES\n"
        "-pld_mp YES\n"
        "-pld_xp YES\n"
        "-pld_ce YES\n"
        "-wysiwyg YES\n"
        "-equivalent_register_removal YES\n"
    >>).

