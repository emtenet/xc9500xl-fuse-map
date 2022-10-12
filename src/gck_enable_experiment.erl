-module(gck_enable_experiment).

-export([run/0]).

%%  Detect GCK enable fuses at the device level.

%%====================================================================
%% run
%%====================================================================

run() ->
    [ run(Density) || Density <- density:list() ],
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    IOs = experiment:shuffle(device:io_macro_cells(Device)),
    GCKs = device:gck_macro_cells(Device),
    {[A, B, C], _} = experiment:pick(3, IOs, GCKs),
    mc(Device, A, IOs, GCKs),
    mc(Device, B, IOs, GCKs),
    mc(Device, C, IOs, GCKs).

%%--------------------------------------------------------------------

mc(Device, MC, IOs, GCKs) ->
    Used = [MC | GCKs],
    {Input, _} = experiment:pick(IOs, Used),
    Clocks = clocks(GCKs, [gck1, gck2, gck3], MC, []),
    Sources = sources(Clocks, MC, Input, []),
    Answers = [ experiment(Device, MC, Source) || Source <- Sources ],
    Matrix0 = matrix:diff(Answers),
    Matrix = matrix:filter_by_name(fun filter/2, #{}, Device, Matrix0),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    fuses(Names, Matrix).

%%--------------------------------------------------------------------

clocks([], _, _, Clocks) ->
    lists:reverse(Clocks);
clocks([MC | GCKs], [_ | GNames], MC, Clocks) ->
    clocks(GCKs, GNames, MC, Clocks);
clocks([GCK | GCKs], [GName | GNames], MC, Clocks) ->
    Clock = {GName, GCK},
    clocks(GCKs, GNames, MC, [Clock | Clocks]).

%%--------------------------------------------------------------------

sources([], MC, Input, Sources) ->
    Bypass = source(on, undefined, MC, Input),
    [Bypass | lists:reverse(Sources)];
sources([{GCK, GMC} | Clocks], MC, Input, Sources) ->
    Source = source(GCK, GMC, MC, Input),
    sources(Clocks, MC, Input, [Source | Sources]).

%%--------------------------------------------------------------------

source(on, undefined, MC, Input) ->
    {on, [
        {i, Input},
        {o, MC, i}
    ]};
source(GCK, GMC, MC, Input) ->
    {GCK, lists:flatten([
        {i, Input},
        {GCK, GMC, #{global => gck}},
        {o, MC, i, #{clk => GCK}}
    ])}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gck-enable ~s ~s ~s~n", [Device, MC, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

filter(gck1_enable, _) ->
    true;
filter(gck2_enable, _) ->
    true;
filter(gck3_enable, _) ->
    true;
filter({FB, MC, Feature}, State0)
        when Feature =:= bypass orelse
             Feature =:= gck_1 orelse
             Feature =:= gck_0 ->
    State1 = match_fb(FB, State0),
    State2 = match_mc(MC, State1),
    {true, State2};
filter({FB, MC, pt3, Input}, State0) ->
    input:assert(Input),
    State1 = match_fb(FB, State0),
    State2 = match_mc(MC, State1),
    {false, State2};
filter({FB, Input, _Mux}, State0) ->
    input:assert(Input),
    State1 = match_fb(FB, State0),
    {false, State1}.

%%--------------------------------------------------------------------

match_fb(FB, State = #{fb := FB}) ->
    State;
match_fb(FB, State) when not is_map_key(fb, State) ->
    State#{fb => FB}.

%%--------------------------------------------------------------------

match_mc(MC, State = #{mc := MC}) ->
    State;
match_mc(MC, State) when not is_map_key(mc, State) ->
    State#{mc => MC}.

%%--------------------------------------------------------------------

fuses([gck1_enable,
       gck2_enable,
       gck3_enable,
       {FB, MC, bypass},
       {FB, MC, gck_1},
       {FB, MC, gck_0}
      ],
      {matrix, _,
       [{on,   [off, off, off, on,  off, off]},
        {gck1, [on,  off, off, off, off, on ]},
        {gck2, [off, on,  off, off, off, off]},
        {gck3, [off, off, on,  off, on,  off]}
       ]
      }) ->
    ok.

