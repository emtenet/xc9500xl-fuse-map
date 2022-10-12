-module(gts_enable_experiment).

-export([run/0]).

%%  Detect GTS# enable fuses at the device level.

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
    GTSs = device:gts_macro_cells(Device),
    [GCK | _] = device:gck_macro_cells(Device),
    {[I, O], _} = experiment:pick(2, IOs, [GCK | GTSs]),
    mc(Device, I, O, GCK, GTSs).

%%--------------------------------------------------------------------

mc(Device, I, O, GCK, GTSs) ->
    Enables = enables(GTSs, [gts1, gts2, gts3, gts4], []),
    Sources = sources(Enables, I, O, GCK, []),
    Answers = [ experiment(Device, I, O, Source) || Source <- Sources ],
    Matrix = matrix:diff(Answers),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    fuses(Names, Matrix).

%%--------------------------------------------------------------------

enables([], _, Enables) ->
    lists:reverse(Enables);
enables([GTS | GTSs], [Name | Names], Enables) ->
    Enable = {Name, GTS},
    enables(GTSs, Names, [Enable | Enables]).

%%--------------------------------------------------------------------

sources([], I, O, GCK, Sources) ->
    Bypass = source(on, undefined, I, O, GCK),
    [Bypass | lists:reverse(Sources)];
sources([{GTS, MC} | Enables], I, O, GCK, Sources) ->
    Source = source(GTS, MC, I, O, GCK),
    sources(Enables, I, O, GCK, [Source | Sources]).

%%--------------------------------------------------------------------

source(on, undefined, I, O, GCK) ->
    {on, [
        {i, I},
        {gck, GCK, #{global => gck}},
        {o, O, i, #{clk => gck}}
    ]};
source(GTS, MC, I, O, GCK) ->
    {GTS, [
        {i, I},
        {gck, GCK, #{global => gck}},
        {GTS, MC, #{global => gts}},
        {o, O, i, #{clk => gck, oe => GTS}}
    ]}.

%%--------------------------------------------------------------------

experiment(Device, I, O, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gts-enable ~s ~s -> ~s ~s~n", [Device, I, O, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

fuses([gts1_enable,
       gts2_enable,
       {FB, MC, oe_gts},
       {FB, MC, oe_gts_0},
       {FB, MC, oe_invert}
      ],
      {matrix, _,
       [{on,   [off, off, off, off, on ]},
        {gts1, [on,  off, on,  off, off]},
        {gts2, [off, on,  on,  on,  off]}
       ]
      }) ->
    ok;
fuses([gts1_enable,
       gts2_enable,
       gts3_enable,
       gts4_enable,
       {FB, MC, oe_gts},
       {FB, MC, oe_gts_0},
       {FB, MC, oe_gts_1},
       {FB, MC, oe_invert}
      ],
      {matrix, _,
       [{on,   [off, off, off, off, off, off, off, on ]},
        {gts1, [on,  off, off, off, on,  off, off, off]},
        {gts2, [off, on,  off, off, on,  on,  off, off]},
        {gts3, [off, off, on,  off, on,  off, on,  off]},
        {gts4, [off, off, off, on,  on,  on,  on,  off]}
       ]
      }) ->
    ok.

