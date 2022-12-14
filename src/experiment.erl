-module(experiment).

-export([shuffle/1]).
-export([pick/2]).
-export([pick/3]).
-export([compile/1]).
-export([run/1]).
-export([cache/1]).
-export([cache_in/2]).
-export([cache_refresh/1]).
-export([cached_imux/1]).
-export([cached_jed/1]).
-export([cached_with/1]).
-export([cache_iterate/0]).
-export([cache_iterate/1]).
-export([pins/0]).
-export([fuse_count/0]).
-export([imux/0]).
-export([jed/0]).
-export([jed_file/1]).

-type with() :: #{
    device := device:device(),
    init => high | low,
    optimize => true | false,
    power => auto | low | std,
    slew => auto | fast | slow,
    terminate => float | keeper,
    unused => float | ground,
    usercode => binary(),
    ucf := binary(),
    vhdl := binary()
}.
-export_type([with/0]).

-type jed() :: [fuse:fuse()].
-export_type([jed/0]).

-type cache() ::
    {cache, miss, with(), file:filename(), jed()} |
    {cache, hit, with(), file:filename()}.
-export_type([cache/0]).

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% shuffle
%%====================================================================

shuffle(Ins) ->
    Pairs = [ {rand:uniform(), In} || In <- Ins ],
    Sorted = lists:sort(Pairs),
    [ Out || {_, Out} <- Sorted ].

%%====================================================================
%% pick
%%====================================================================

pick([Item | Items], Except) ->
    case lists:member(Item, Except) of
        true ->
            pick(Items, Except);

        false ->
            {Item, Items}
    end.

%%--------------------------------------------------------------------

pick(Count, Items, Except) when is_integer(Count) andalso Count > 0 ->
    pick(Count, Items, Except, []).

%%--------------------------------------------------------------------

pick(0, Items, _, Picked) ->
    {lists:reverse(Picked), Items};
pick(Count, [Item | Items], Except, Picked) ->
    case lists:member(Item, Except) of
        true ->
            pick(Count, Items, Except, Picked);

        false ->
            pick(Count - 1, Items, Except, [Item | Picked])
    end.

%%====================================================================
%% compile
%%====================================================================

-type input() :: #{
        global := global()
    }.
-type global() :: gck | gsr | gts.
-type internal() :: #{
        init => 0 | 1,
        power => std | low,
        type => d | t,
        clk => logic(),
        ce => logic(),
        s => logic(),
        r => logic()
    }.
-type output() :: #{
        init => 0 | 1,
        power => std | low,
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

compile_ucf({Net, MC, Options = #{}}) ->
    compile_ucf(Net, MC, compile_ucf_global(Options), <<>>);
compile_ucf({Net, MC}) ->
    compile_ucf(Net, MC, <<>>, <<>>);
compile_ucf({Net, MC, _}) ->
    compile_ucf(Net, MC, <<>>, <<>>);
compile_ucf({Net, MC, _, Options = #{}}) ->
    compile_ucf(Net, MC, <<>>, compile_ucf_power(Options));
compile_ucf({Net, MC, _, internal, Options = #{}}) ->
    compile_ucf(Net, MC, <<>>, compile_ucf_power(Options)).

%%--------------------------------------------------------------------

compile_ucf_global(#{global := gck}) ->
    <<" | BUFG=CLK">>;
compile_ucf_global(#{global := gsr}) ->
    <<" | BUFG=SR">>;
compile_ucf_global(#{global := gts}) ->
    <<" | BUFG=OE">>;
compile_ucf_global(#{}) ->
    <<>>.

%%--------------------------------------------------------------------

compile_ucf_power(#{power := std}) ->
    <<" | PWR_MODE = STD">>;
compile_ucf_power(#{}) ->
    <<>>.

%%--------------------------------------------------------------------

compile_ucf(Net_, MC_, Global, Power) ->
    Net = compile_net(Net_),
    MC = macro_cell:name(MC_),
    <<"NET \"", Net/binary, "\""
      " LOC = \"", MC/binary, "\"",
      Global/binary,
      Power/binary,
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
    {Lines0, Q} = compile_ff_oe(Output, FF),
    {Lines1, D} = compile_ff_d(Output, Logic_, Lines0),
    lists:reverse(Lines1, [compile_ff(Output, Q, D, FF)]);
compile_vhdl({Output_, _, Logic, #{oe := OE}}) ->
    Output = compile_net(Output_),
    compile_oe(Output, Logic, Output, OE);
compile_vhdl({Output_, _, Logic_, #{}}) ->
    Output = compile_net(Output_),
    Logic = compile_logic(Logic_),
    <<"  ", Output/binary, " <= ", Logic/binary, ";\n">>;
compile_vhdl({Output_, _, Logic_, internal, FF = #{}}) ->
    Output = compile_net(Output_),
    Q = Output,
    Lines0 = [],
    {Lines1, D} = compile_ff_d(Output, Logic_, Lines0),
    lists:reverse(Lines1, [compile_ff(Output, Q, D, FF)]).

%%--------------------------------------------------------------------

compile_logic(true) ->
    <<"'0'">>;
compile_logic(false) ->
    <<"'1'">>;
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

compile_signal({Output_, _, Logic_, #{clk := _, oe := _}}) ->
    Output = compile_net(Output_),
    {true, [
        <<"  signal ", Output/binary, "_Q : STD_LOGIC;\n">>,
        compile_signal_d(Output, Logic_)
    ]};
compile_signal({Output_, _, Logic_, #{clk := _}}) ->
    Output = compile_net(Output_),
    Signals = [
        compile_signal_d(Output, Logic_)
    ],
    case Signals of
        [<<>>] ->
            false;

        _ ->
            {true, Signals}
    end;
compile_signal({Output_, _, Logic_, internal, #{}}) ->
    Output = compile_net(Output_),
    {true, [
        <<"  signal ", Output/binary, " : STD_LOGIC;\n">>,
        compile_signal_d(Output, Logic_)
    ]};
compile_signal(_) ->
    false.

%%--------------------------------------------------------------------

compile_signal_d(_Net, Logic) when is_atom(Logic) ->
    <<>>;
compile_signal_d(Net, _) ->
    <<"  signal ", Net/binary, "_D : STD_LOGIC;\n">>.

%%--------------------------------------------------------------------

compile_ff(Name, Q, D, FF = #{clk := Clk_}) ->
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

compile_ff_d(_Net, Logic_, Lines) when is_atom(Logic_) ->
    Logic = compile_logic(Logic_),
    {Lines, Logic};
compile_ff_d(Net, Logic_, Lines) ->
    Logic = compile_logic(Logic_),
    D = <<Net/binary, "_D">>,
    Line = <<"  ", Net/binary, "_D <= ", Logic/binary, ";\n">>,
    {[Line | Lines], D}.

%%--------------------------------------------------------------------

compile_ff_oe(Output, #{oe := true}) ->
    Q = <<Output/binary, "_Q">>,
    OE = <<"  ", Output/binary, " <= ", Q/binary, ";\n">>,
    {[OE], Q};
compile_ff_oe(Output, #{oe := OE_}) ->
    Q = <<Output/binary, "_Q">>,
    OE = compile_oe(Output, Q, Output, OE_),
    {[OE], Q};
compile_ff_oe(Output, _) ->
    {[], Output}.

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

internal(RunIn, With = #{device := Device}) ->
    RunDir = filename:join("experiment", RunIn),
    make_dir(RunDir),
    clear_dir(RunDir),
    make_tmp(RunDir),
    make_prj(RunDir),
    make_xst(RunDir),
    make_ucf(RunDir, With),
    make_vhdl(RunDir, With),
    ok = exec(RunDir, "xst", [
        "-intstyle", "ise",
        "-ifn", "experiment.xst"
    ]),
    ok = exec(RunDir, "ngdbuild", [
        "experiment.ngc",
        "experiment.ngd",
        "-intstyle", "ise",
        "-dd", "_ngo",
        "-uc", "experiment.ucf",
        "-p", device:name(Device)
    ]),
    ok = exec(RunDir, "cpldfit", [
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
    ok = exec(RunDir, "hprep6", [
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
    internal("manual", with_defaults(With)).

%%====================================================================
%% cache
%%====================================================================

cache(With) ->
    cache_in("cache", With).

%%--------------------------------------------------------------------

cache_in(RunIn, With0) ->
    With = with_defaults(With0),
    Query = cache_query(With),
    CacheDir = cache_dir(Query),
    %io:format("CACHE ~s~n", [CacheDir]),
    case cache_read_query(CacheDir) of
        {ok, Query} ->
            {cache, hit, With, CacheDir};

        {error, enoent} ->
            cache_miss(RunIn, With, CacheDir, Query)
    end.

%%--------------------------------------------------------------------

cache_refresh(Cache = {cache, miss, _, _, _}) ->
    Cache;
cache_refresh({cache, hit, With, CacheDir}) ->
    cache_delete_query(CacheDir),
    Query = cache_query(With),
    cache_miss("refresh", With, CacheDir, Query).

%%--------------------------------------------------------------------

cache_miss(RunIn, With, CacheDir, Query) ->
    internal(RunIn, With),
    cache_copy(RunIn, CacheDir, "experiment.jed"),
    cache_copy(RunIn, CacheDir, "experiment_pad.csv"),
    cache_copy(RunIn, CacheDir, "experiment.rpt"),
    cache_copy(RunIn, CacheDir, "experiment.vm6"),
    JED = jed(RunIn),
    cache_write_jed(CacheDir, JED),
    cache_write_query(CacheDir, Query),
    {cache, miss, With, CacheDir, JED}.

%%--------------------------------------------------------------------

cache_query(With) ->
    iolist_to_binary(io_lib:format("~p.", [With])).

%%--------------------------------------------------------------------

cache_dir(Query) ->
    Hash = crypto:hash(sha256, Query),
    Base64 = base64url:encode(Hash),
    cache_make_dir(filename:join("cache", Base64)).

%%--------------------------------------------------------------------

cache_make_dir(CacheDir) ->
    case file:make_dir(CacheDir) of
        ok ->
            CacheDir;

        {error, eexist} ->
            CacheDir
    end.

%%--------------------------------------------------------------------

cache_consult_query(CacheDir) ->
    File = filename:join(CacheDir, "with"),
    case file:consult(File) of
        {ok, [With]} ->
            {ok, With};

        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------

cache_delete_query(CacheDir) ->
    File = filename:join(CacheDir, "with"),
    ok = file:delete(File).

%%--------------------------------------------------------------------

cache_read_query(CacheDir) ->
    File = filename:join(CacheDir, "with"),
    file:read_file(File).

%%--------------------------------------------------------------------

cache_copy(RunIn, CacheDir, Name) ->
    Source = filename:join(["experiment", RunIn, Name]),
    Destination = filename:join(CacheDir, Name),
    {ok, _} = file:copy(Source, Destination),
    ok.

%%--------------------------------------------------------------------

cache_write_jed(CacheDir, JED) ->
    File = filename:join(CacheDir, "jed"),
    Data = io_lib:format("~p.", [JED]),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

cache_write_query(CacheDir, Query) ->
    File = filename:join(CacheDir, "with"),
    ok = file:write_file(File, Query).

%%====================================================================
%% cached_dir
%%====================================================================

cached_dir({cache, hit, _With, CacheDir}) ->
    CacheDir;
cached_dir({cache, miss, _With, CacheDir, _JED}) ->
    CacheDir.

%%====================================================================
%% cached_imux
%%====================================================================

cached_imux(Cache) ->
    File = filename:join(cached_dir(Cache), "experiment.vm6"),
    experiment_vm6:imux(File).

%%====================================================================
%% cached_jed
%%====================================================================

cached_jed({cache, hit, _With, CacheDir}) ->
    File = filename:join(CacheDir, "jed"),
    {ok, [JED]} = file:consult(File),
    JED;
cached_jed({cache, miss, _With, _CacheDir, JED}) ->
    JED.

%%====================================================================
%% cached_with
%%====================================================================

cached_with({cache, hit, With, _CacheDir}) ->
    With;
cached_with({cache, miss, With, _CacheDir, _JED}) ->
    With.

%%====================================================================
%% cache_iterate
%%====================================================================

-type cache_iterate() :: {cache_iterate, [file:filename()]}.

-spec cache_iterate() -> false | {cache(), cache_iterate()}.

cache_iterate() ->
    {ok, CacheDirs} = file:list_dir("cache"),
    cache_iterate_dirs(CacheDirs).

%%--------------------------------------------------------------------

-spec cache_iterate(cache_iterate()) -> false | {cache(), cache_iterate()}.

cache_iterate({cache_iterate, CacheDirs}) ->
    cache_iterate_dirs(CacheDirs).

%%--------------------------------------------------------------------

cache_iterate_dirs([]) ->
    false;
cache_iterate_dirs([CacheDir | CacheDirs]) ->
    case cache_iterate_dir(CacheDir) of
        {ok, Cache} ->
            {Cache, {cache_iterate, CacheDirs}};

        false ->
            cache_iterate_dirs(CacheDirs)
    end.

%%--------------------------------------------------------------------

cache_iterate_dir(Hash) ->
    CacheDir = filename:join("cache", Hash),
    case cache_consult_query(CacheDir) of
        {ok, With} ->
            {ok, {cache, hit, With, CacheDir}};

        _ ->
            false
    end.

%%====================================================================
%% pins
%%====================================================================

pins() ->
    pins("manual").

%%--------------------------------------------------------------------

pins(RunIn) ->
    File = filename:join(["experiment", RunIn, "experiment_pad.csv"]),
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    pin_lines(Lines).

%%--------------------------------------------------------------------

pin_lines([<<"Pin Number,Signal Name,Pin Usage,Pin Name,Direction,", _/binary>> | Lines]) ->
    pin_lines(Lines, []);
pin_lines([_ | Lines]) ->
    pin_lines(Lines).

%%--------------------------------------------------------------------

pin_lines([<<>> | _], Pads) ->
    lists:reverse(Pads);
pin_lines([Line | Lines], Pads) ->
    pin_lines(Lines, [pin_line(Line) | Pads]).

%%--------------------------------------------------------------------

pin_line(Line) ->
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
    RunIn = "manual",
    File = filename:join(["experiment", RunIn, "experiment.jed"]),
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
    imux("manual").

%%--------------------------------------------------------------------

imux(RunIn) ->
    File = filename:join(["experiment", RunIn, "experiment.vm6"]),
    experiment_vm6:imux(File).

%%====================================================================
%% jed
%%====================================================================

jed() ->
    jed("manual").

%%--------------------------------------------------------------------

jed_file(File) ->
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    jed(Lines, []).

%%--------------------------------------------------------------------

jed(RunIn) ->
    File = filename:join(["experiment", RunIn, "experiment.jed"]),
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

exec(RunDir, Arg0, Args) ->
    Path = "/home/Xilinx/12.4/ISE_DS/ISE/bin/lin64",
    Exec = {spawn_executable, filename:join(Path, Arg0)},
    Opts = [
        {args, Args},
        {cd, RunDir},
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

