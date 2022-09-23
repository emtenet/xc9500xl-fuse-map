-module(gck_experiment).

-export([run/0]).

%%  Detect GCK & bypass fuses
%%
%%  A macro-cell can be combinatorial, the flip-flop is "bypassed".
%%
%%  Or a macro-cell can be registered, with the flip-flop clock
%%  provided from:
%%
%%    * GCK1
%%    * GCK2
%%    * GCK3
%%    * a product-term
%%
%%  We do not attempt to detect a product-term clock.

%%====================================================================
%% run
%%====================================================================

run() ->
    [ run(Density) || Density <- density:list() ],
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    GCKs = device:gck_macro_cells(Device),
    Fuses = lists:flatten([ mc(Device, IO, IOs, GCKs) || IO <- IOs ]),
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

mc(Device, MC, IOs, GCKs) ->
    Used = [MC | GCKs],
    {Input, Avail} = input(IOs, Used),
    Controls = controls(
        GCKs,
        [gck1, gck2, gck3],
        [x, y, z],
        MC, Avail, Used,
        []
    ),
    Sources = sources(Controls, MC, Input, Controls, []),
    Answers = [ experiment(Device, MC, Source) || Source <- Sources ],
    Matrix = fuses:matrix(Answers),
    fuses:print(Matrix),
    fuses(MC, Matrix).

%%--------------------------------------------------------------------

input([IO | IOs], Used) ->
    case lists:member(IO, Used) of
        true ->
            input(IOs, Used);

        false ->
            {IO, IOs}
    end.

%%--------------------------------------------------------------------

controls([], _, _, _, _, _, Controls) ->
    lists:reverse(Controls);
controls([MC | GMCs], [_ | GNames], [_ | Names], MC, Avail, Used, Controls) ->
    controls(GMCs, GNames, Names, MC, Avail, Used, Controls);
controls([GMC | GMCs], [GName | GNames], [Name | Names], MC, Avail0, Used, Controls) ->
    {Control, Avail} = input(Avail0, Used),
    Option = {GName, GMC, Name, Control},
    controls(GMCs, GNames, Names, MC, Avail, Used, [Option | Controls]).

%%--------------------------------------------------------------------

sources([], MC, Input, Controls, Sources) ->
    Source = source(on, MC, Input, Controls),
    [Source | lists:reverse(Sources)];
sources([{GCK, _, _, _} | Clocks], MC, Input, Controls, Sources) ->
    Source = source(GCK, MC, Input, Controls),
    sources(Clocks, MC, Input, Controls, [Source | Sources]).

%%--------------------------------------------------------------------

source(GCK, MC, Input, Controls_) ->
    Detect = case GCK of
        on ->
            {o, MC, i};

        _ ->
            {o, MC, i, #{clk => GCK}}
    end,
    Clocks = [
        {Net, Loc}
        ||
        {Net, Loc, _, _} <- Controls_
    ],
    Controls = [
        {Net, Loc, i, #{clk => Clock}}
        ||
        {Clock, _, Net, Loc} <- Controls_
    ],
    {GCK, lists:flatten([
        {i, Input},
        Clocks,
        Detect,
        Controls
    ])}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gck ~s ~s ~s~n", [Device, MC, Name]),
    {Name, experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    })}.

%%--------------------------------------------------------------------

fuses(MC, {matrix,
           [BYPASS, GCK3, GCK1],
           [{on,   [on , off, off]},
            {gck1, [off, off, on ]},
            {gck2, [off, off, off]},
            {gck3, [off, on , off]}
           ]
          }) ->
    [fuse(BYPASS, MC, ff_bypass),
     fuse(GCK1, MC, ff_gck1),
     fuse(GCK3, MC, ff_gck3)
    ];
fuses(MC, {matrix,
           [BYPASS, GCK3],
           [{on,   [on , off]},
            {gck2, [off, off]},
            {gck3, [off, on ]}
           ]
          }) ->
    [fuse(BYPASS, MC, ff_bypass),
     fuse(GCK3, MC, ff_gck3)
    ];
fuses(MC, {matrix,
           [BYPASS, GCK3, GCK1],
           [{on,   [on , off, off]},
            {gck1, [off, off, on ]},
            {gck3, [off, on , off]}
           ]
          }) ->
    [fuse(BYPASS, MC, ff_bypass),
     fuse(GCK1, MC, ff_gck1),
     fuse(GCK3, MC, ff_gck3)
    ];
fuses(MC, {matrix,
           [BYPASS, GCK1],
           [{on,   [on , off]},
            {gck1, [off, on ]},
            {gck2, [off, off]}
           ]
          }) ->
    [fuse(BYPASS, MC, ff_bypass),
     fuse(GCK1, MC, ff_gck1)
    ].

%%--------------------------------------------------------------------

fuse(Fuse, MC, Extra) ->
    {Fuse, list_to_atom(io_lib:format("~s_~s", [MC, Extra]))}.

