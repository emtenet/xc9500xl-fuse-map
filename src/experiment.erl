-module(experiment).

-export([compile/1]).
-export([run/1]).
-export([cache/1]).
-export([cache_refresh/1]).
-export([cached_imux/1]).
-export([cached_jed/1]).
-export([pins/0]).
-export([fuse_count/0]).
-export([imux/0]).
-export([jed/0]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% compile
%%====================================================================

-type input() :: #{
        global => global()
    }.
-type global() :: gck | gsr | gts.
-type internal() :: #{
        init => 0 | 1,
        type => d | t,
        clk => logic(),
        ce => logic(),
        s => logic(),
        r => logic()
    }.
-type output() :: #{
        init => 0 | 1,
        type => d | t,
        clk => logic(),
        ce => logic(),
        s => logic(),
        r => logic(),
        oe => logic()
    }.
-type net() :: atom().
-type mc() :: atom().
-type logic() :: atom() | {low, atom()} | binary().
-type signal() ::
    % input
    {net(), mc()} |
    {net(), mc(), input()} |
    % internal
    {net(), mc(), logic(), internal, internal()}  |
    % output
    {net(), mc(), logic()} |
    {net(), mc(), logic(), output()}.
-spec compile([signal()]) -> {binary(), binary()}.

compile(Signals) ->
    UCF = iolist_to_binary([
        compile_ucf(Signal)
        ||
        Signal <- Signals
    ]),
    Ports = [
        compile_port(Signal)
        ||
        Signal <- Signals,
        not compile_internal(Signal)
    ],
    Library = compile_library(Signals),
    Logic = [
        compile_vhdl(Signal)
        ||
        Signal <- Signals,
        not compile_input(Signal)
    ],
    VHDL = iolist_to_binary([<<
        "library IEEE;\n"
        "use IEEE.STD_LOGIC_1164.ALL;\n">>,
        Library, <<
        "\n"
        "entity experiment is\n"
        "  port (\n">>,
        lists:join(<<";\n">>, Ports), <<"\n"
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is">>,
        compile_signals(Signals), <<
        "begin\n">>,
        Logic, <<
        "end behavioral;\n"
    >>]),
    {UCF, VHDL}.

%%--------------------------------------------------------------------

compile_library([]) ->
    <<>>;
compile_library([{_, _, _, _} | _]) ->
    compile_library();
compile_library([{_, _, _, internal, _} | _]) ->
    compile_library();
compile_library([_ | Signals]) ->
    compile_library(Signals).

%%--------------------------------------------------------------------

compile_library() ->
    <<
        "library UNISIM;\n"
        "use UNISIM.vcomponents.ALL;\n"
    >>.

%%--------------------------------------------------------------------

compile_net(Net) ->
    atom_to_binary(Net, latin1).

%%--------------------------------------------------------------------

compile_input({_, _}) ->
    true;
compile_input({_, _, #{}}) ->
    true;
compile_input(_) ->
    false.

%%--------------------------------------------------------------------

compile_internal({_, _, _, internal, _}) ->
    true;
compile_internal(_) ->
    false.

%%--------------------------------------------------------------------

compile_ucf({Net, MC, #{global := gck}}) ->
    compile_ucf(Net, MC, <<" | BUFG=CLK">>);
compile_ucf({Net, MC, #{global := gsr}}) ->
    compile_ucf(Net, MC, <<" | BUFG=SR">>);
compile_ucf({Net, MC, #{global := gts}}) ->
    compile_ucf(Net, MC, <<" | BUFG=OE">>);
compile_ucf({Net, MC}) ->
    compile_ucf(Net, MC, <<>>);
compile_ucf({Net, MC, _}) ->
    compile_ucf(Net, MC, <<>>);
compile_ucf({Net, MC, _, _}) ->
    compile_ucf(Net, MC, <<>>);
compile_ucf({Net, MC, _, internal, _}) ->
    compile_ucf(Net, MC, <<>>).

%%--------------------------------------------------------------------

compile_ucf(Net_, MC_, Global) ->
    Net = compile_net(Net_),
    MC = macro_cell:name(MC_),
    <<"NET \"", Net/binary, "\""
      " LOC = \"", MC/binary, "\"",
      Global/binary,
      ";\n"
    >>.

%%--------------------------------------------------------------------

compile_port({Net, _}) ->
    compile_port(Net, <<"in">>);
compile_port({Net, _, #{global := gck}}) ->
    compile_port(Net, <<"in">>);
compile_port({Net, _, #{global := gsr}}) ->
    compile_port(Net, <<"in">>);
compile_port({Net, _, #{global := gts}}) ->
    compile_port(Net, <<"in">>);
compile_port({Net, _, #{}}) ->
    compile_port(Net, <<"in">>);
compile_port({Net, _, _}) ->
    compile_port(Net, <<"out">>);
compile_port({Net, _, _, _}) ->
    compile_port(Net, <<"out">>).

%%--------------------------------------------------------------------

compile_port(Net_, Dir) ->
    Net = compile_net(Net_),
    <<"    ", Net/binary, " : ", Dir/binary, " STD_LOGIC">>.

%%--------------------------------------------------------------------

compile_vhdl({Output_, _, Logic_}) ->
    Output = compile_net(Output_),
    Logic = compile_logic(Logic_),
    <<"  ", Output/binary, " <= ", Logic/binary, ";\n">>;
compile_vhdl({Output_, _, Logic_, FF = #{clk := _}}) ->
    Output = compile_net(Output_),
    case compile_ff_oe(Output, FF) of
        {OE, Q} ->
            [OE, compile_ff(Output, Q, Logic_, FF)];

        false ->
            compile_ff(Output, Output, Logic_, FF)
    end;
compile_vhdl({Output_, _, Logic, X = #{oe := OE}}) when map_size(X) =:= 1 ->
    Output = compile_net(Output_),
    compile_oe(Output, Logic, Output, OE);
compile_vhdl({Output_, _, Logic_, #{}}) ->
    Output = compile_net(Output_),
    Logic = compile_logic(Logic_),
    <<"  ", Output/binary, " <= ", Logic/binary, ";\n">>;
compile_vhdl({Internal_, _, Logic_, internal, FF = #{}}) ->
    Internal = compile_net(Internal_),
    compile_ff(Internal, Internal, Logic_, FF).

%%--------------------------------------------------------------------

compile_logic(Net) when is_atom(Net) ->
    compile_net(Net);
compile_logic({low, Net}) when is_atom(Net) ->
    <<"NOT ", (compile_net(Net))/binary>>;
compile_logic(Logic) when is_binary(Logic) ->
    Logic.

%%--------------------------------------------------------------------

compile_init(#{init := Init}) ->
    case Init of
        0 -> <<"'0'">>;
        1 -> <<"'1'">>
    end;
compile_init(#{}) ->
    <<"'0'">>.

%%--------------------------------------------------------------------

compile_signals(Signals) ->
    case lists:filtermap(fun compile_signal/1, Signals) of
        [] ->
            <<" ">>;

        Code ->
            [<<"\n">> | Code]
    end.

%%--------------------------------------------------------------------

compile_signal({Output_, _, _, #{clk := _, oe := _}}) ->
    Output = compile_net(Output_),
    {true, <<
        "  signal ", Output/binary, "_Q : STD_LOGIC;\n"
    >>};
compile_signal({Internal_, _, _, internal, #{}}) ->
    Internal = compile_net(Internal_),
    {true, <<
        "  signal ", Internal/binary, " : STD_LOGIC;\n"
    >>};
compile_signal(_) ->
    false.

%%--------------------------------------------------------------------

compile_ff(Name, Q, D_, FF = #{clk := Clk_}) ->
    D = compile_logic(D_),
    Clk = compile_logic(Clk_),
    Init = compile_init(FF),
    case compile_ff_type(FF) of
        {d, Type, S, R, CE} ->
            <<
                "  ", Name/binary, "_FF: ", Type/binary,
                    " generic map (", Init/binary, ")"
                    " port map (\n",
                S/binary, R/binary, CE/binary,
                "    D => ", D/binary, ",\n"
                "    Q => ", Q/binary, ",\n"
                "    C => ", Clk/binary, "\n"
                "  );\n"
            >>;

        {t, Type, S, R, CE} ->
            <<
                "  ", Name/binary, "_FF: ", Type/binary,
                    " generic map (", Init/binary, ")"
                    " port map (\n",
                S/binary, R/binary, CE/binary,
                "    T => ", D/binary, ",\n"
                "    Q => ", Q/binary, ",\n"
                "    C => ", Clk/binary, "\n"
                "  );\n"
            >>
    end.

%%--------------------------------------------------------------------

compile_ff_oe(Output, #{oe := OE_}) ->
    Q = <<Output/binary, "_Q">>,
    OE = compile_oe(Output, Q, Output, OE_),
    {OE, Q};
compile_ff_oe(_, _) ->
    false.

%%--------------------------------------------------------------------

compile_oe(Name, Input_, Output_, OE_) ->
    Input = compile_logic(Input_),
    Output = compile_logic(Output_),
    OE = compile_logic(OE_),
    <<
        "  ", Name/binary, "_OE: OBUFE port map (\n"
        "    I => ", Input/binary, ",\n"
        "    O => ", Output/binary, ",\n"
        "    E => ", OE/binary, "\n"
        "  );\n"
    >>.

%%--------------------------------------------------------------------

compile_ff_type(FF = #{type := Type}) ->
    case Type of
        d -> compile_d_type(FF);
        t -> compile_t_type(FF)
    end;
compile_ff_type(FF) ->
    compile_d_type(FF).

%%--------------------------------------------------------------------

compile_d_type(#{s := S_, r := R_, ce := CE_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {d, <<"FDCPE">>, S, R, CE};
compile_d_type(#{s := S_, r := R_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    {d, <<"FDCP">>, S, R, <<>>};
compile_d_type(#{s := S_, ce := CE_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {d, <<"FDPE">>, S, <<>>, CE};
compile_d_type(#{s := S_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    {d, <<"FDP">>, S, <<>>, <<>>};
compile_d_type(#{r := R_, ce := CE_}) ->
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {d, <<"FDCE">>, <<>>, R, CE};
compile_d_type(#{r := R_}) ->
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    {d, <<"FDC">>, <<>>, R, <<>>};
compile_d_type(#{ce := CE_}) ->
    R = <<"    CLR => '0',\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {d, <<"FDCE">>, <<>>, R, CE};
compile_d_type(#{}) ->
    {d, <<"FD">>, <<>>, <<>>, <<>>}.

%%--------------------------------------------------------------------

compile_t_type(#{s := S_, r := R_, ce := CE_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {t, <<"FTCPE">>, S, R, CE};
compile_t_type(#{s := S_, r := R_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    {t, <<"FTCP">>, S, R, <<>>};
compile_t_type(#{s := S_, ce := CE_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {t, <<"FTPE">>, S, <<>>, CE};
compile_t_type(#{s := S_}) ->
    S = <<"    PRE => ", (compile_logic(S_))/binary, ",\n">>,
    {t, <<"FTP">>, S, <<>>, <<>>};
compile_t_type(#{r := R_, ce := CE_}) ->
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {t, <<"FTCE">>, <<>>, R, CE};
compile_t_type(#{r := R_}) ->
    S = <<"    PRE => '0',\n">>,
    R = <<"    CLR => ", (compile_logic(R_))/binary, ",\n">>,
    {t, <<"FTCP">>, S, R, <<>>};
compile_t_type(#{ce := CE_}) ->
    R = <<"    CLR => '0',\n">>,
    CE = <<"    CE => ", (compile_logic(CE_))/binary, ",\n">>,
    {t, <<"FTCE">>, <<>>, R, CE};
compile_t_type(#{}) ->
    S = <<"    PRE => '0',\n">>,
    R = <<"    CLR => '0',\n">>,
    {t, <<"FTCP">>, S, R, <<>>}.

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
        "-nogclkopt", % Disable Global Clock optimization.
        "-nogsropt", % Disable Global Set/Reset optimization.
        "-nogtsopt", % Disable Global Output-Enable (GTS) optimization.
        "-slew", with_slew(With),
        "-init", with_init(With),
        "-inputs", "54",
        "-keepio",
        "-pterms", "50",
        "-unused", with_unused(With),
        "-power", with_power(With),
        "-terminate", with_terminate(With)
        |
        with_optimize(With)
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
        optimize => false,
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

with_optimize(#{optimize := true}) ->
    [];
with_optimize(#{optimize := false}) ->
    % Disable multi-level logic optimization.
    ["-nomlopt"].

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
    %io:format("CACHE ~s~n", [Dir]),
    case cache_read_query(Dir) of
        {ok, Query} ->
            {cache, hit, With, Dir};

        {error, enoent} ->
            cache_miss(With, Dir, Query)
    end.

%%--------------------------------------------------------------------

cache_refresh(Cache = {cache, miss, _, _, _}) ->
    Cache;
cache_refresh({cache, hit, With, Dir}) ->
    cache_delete_query(Dir),
    Query = cache_query(With),
    cache_miss(With, Dir, Query).

%%--------------------------------------------------------------------

cache_miss(With, Dir, Query) ->
    internal(With),
    cache_copy(Dir, "experiment.jed"),
    cache_copy(Dir, "experiment_pad.csv"),
    cache_copy(Dir, "experiment.rpt"),
    cache_copy(Dir, "experiment.vm6"),
    JED = jed(),
    cache_write_jed(Dir, JED),
    cache_write_query(Dir, Query),
    {cache, miss, With, Dir, JED}.

%%--------------------------------------------------------------------

cache_query(With) ->
    iolist_to_binary(io_lib:format("~p.", [With])).

%%--------------------------------------------------------------------

cache_dir(Query) ->
    Hash = crypto:hash(sha256, Query),
    Base64 = base64url:encode(Hash),
    cache_make_dir(filename:join("cache", Base64)).

%%--------------------------------------------------------------------

cache_make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            Dir;

        {error, eexist} ->
            Dir
    end.

%%--------------------------------------------------------------------

cache_delete_query(Dir) ->
    File = filename:join(Dir, "with"),
    ok = file:delete(File).

%%--------------------------------------------------------------------

cache_read_query(Dir) ->
    File = filename:join(Dir, "with"),
    file:read_file(File).

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
%% cached_dir
%%====================================================================

cached_dir({cache, hit, _With, Dir}) ->
    Dir;
cached_dir({cache, miss, _With, Dir, _JED}) ->
    Dir.

%%====================================================================
%% cached_imux
%%====================================================================

cached_imux(Cache) ->
    File = filename:join(cached_dir(Cache), "experiment.vm6"),
    experiment_vm6:imux(File).

%%====================================================================
%% cached_jed
%%====================================================================

cached_jed({cache, hit, _With, Dir}) ->
    File = filename:join(Dir, "jed"),
    {ok, [JED]} = file:consult(File),
    JED;
cached_jed({cache, miss, _With, _Dir, JED}) ->
    JED.

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
%% imux
%%====================================================================

imux() ->
    File = filename:join(dir(), "experiment.vm6"),
    experiment_vm6:imux(File).

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

