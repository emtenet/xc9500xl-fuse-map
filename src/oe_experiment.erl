-module(oe_experiment).

-export([run/0]).

%%  Detect OE fuses
%%
%%  The default OE is '0' (disabled).
%%
%%  oe_invert::
%%      Invert the OE signal/
%%      On it's own this inverts the default from disabled to enabled.
%%
%%  oe_mux::
%%      Use one of the global OE pins (GTS1, GTS2, GTS3 or GTS4)
%%
%%  oe_gts_mux0 & oe_gts_mux1::
%%      Two-bit binary selecting one the of 4 global OE pins.
%%
%%      gts_1 gts_0 -> pin
%%        0     0      gts1
%%        0     1      gts2
%%        1     0      gts3
%%        1     1      gts4

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
    OEs = device:gts_macro_cells(Device),
    Fuses = lists:flatten([ mc(Device, IO, IOs, OEs) || IO <- IOs ]),
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

mc(Device, MC, IOs, OEs) ->
    Used = [MC | OEs],
    {Input, Avail} = input(IOs, Used),
    Options = options(
        OEs,
        [gts1, gts2, gts3, gts4],
        [w, x, y, z],
        MC, Avail, Used,
        []
    ),
    Sources = sources(Options, MC, Input, Options, []),
    Answers = [ experiment(Device, MC, Source) || Source <- Sources ],
    Matrix = matrix:diff(Answers),
    matrix:print(Matrix),
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

options([], _, _, _, _, _, Options) ->
    lists:reverse(Options);
options([MC | OEs], [_ | GTSs], [_ | Names], MC, Avail, Used, Options) ->
    options(OEs, GTSs, Names, MC, Avail, Used, Options);
options([OE | OEs], [GTS | GTSs], [Name | Names], MC, Avail0, Used, Options) ->
    {Input, Avail} = input(Avail0, Used),
    Option = {GTS, OE, Name, Input},
    options(OEs, GTSs, Names, MC, Avail, Used, [Option | Options]).

%%--------------------------------------------------------------------

sources([], MC, Input, OEs, Sources) ->
    Source = source(on, always, MC, Input, OEs),
    [Source | lists:reverse(Sources)];
sources([{GTS, _, _, _} | Options], MC, Input, OEs, Sources) ->
    High = source(GTS, high, MC, Input, OEs),
    Low = source(GTS, low, MC, Input, OEs),
    sources(Options, MC, Input, OEs, [Low, High | Sources]).

%%--------------------------------------------------------------------

source(GTS, Low, MC, Input, OEs) ->
    Detect = case GTS of
        on ->
            {v, MC, a};

        _ when Low =:= low ->
            {v, MC, a, #{oe => {low, GTS}}};

        _ ->
            {v, MC, a, #{oe => GTS}}
    end,
    Enables = [
        {OE, Loc, #{global => gts}}
        ||
        {OE, Loc, _, _} <- OEs
    ],
    Controls = [
        {Net, Loc, a, #{oe => OE}}
        ||
        {OE, _, Net, Loc} <- OEs
    ],
    Name = case GTS of
        on -> on;
        gts1 when Low =:= low -> gts1_low;
        gts2 when Low =:= low -> gts2_low;
        gts3 when Low =:= low -> gts3_low;
        gts4 when Low =:= low -> gts4_low;
        _ -> GTS
    end,
    {Name, lists:flatten([
        {a, Input},
        Enables,
        Detect,
        Controls
    ])}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Pins}) ->
    {UCF, VHDL} = experiment:compile(Pins),
    io:format(" => oe ~s ~s ~s~n", [Device, MC, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

fuses(MC, {matrix,
           [GTS, GTS_0, OE],
           [{on,       [off, off, on ]},
            {gts1,     [on , off, off]},
            {gts1_low, [on , off, on ]},
            {gts2,     [on , on , off]},
            {gts2_low, [on , on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, OE],
           [{on,       [off, on ]},
            {gts1,     [on , off]},
            {gts1_low, [on , on ]}
           ]
          }) ->
    % no evidence of the GTS_0 fuse
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, OE],
           [{on,       [off, off, on ]},
            {gts2,     [on , on , off]},
            {gts2_low, [on , on , on ]}
           ]
          }) ->
    % GTS and GTS0 fuses could be the other way around?
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, GTS_1, OE],
           [{on,       [off, off, off, on ]},
            {gts1,     [on , off, off, off]},
            {gts1_low, [on , off, off, on ]},
            {gts2,     [on , on , off, off]},
            {gts2_low, [on , on , off, on ]},
            {gts3,     [on , off, on , off]},
            {gts3_low, [on , off, on , on ]},
            {gts4,     [on , on , on , off]},
            {gts4_low, [on , on , on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_1, MC, oe_gts_mux1),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, GTS_1, OE],
           [{on,       [off, off, off, on ]},
            {gts2,     [on , on , off, off]},
            {gts2_low, [on , on , off, on ]},
            {gts3,     [on , off, on , off]},
            {gts3_low, [on , off, on , on ]},
            {gts4,     [on , on , on , off]},
            {gts4_low, [on , on , on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_1, MC, oe_gts_mux1),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, GTS_1, OE],
           [{on,       [off, off, off, on ]},
            {gts1,     [on , off, off, off]},
            {gts1_low, [on , off, off, on ]},
            {gts3,     [on , off, on , off]},
            {gts3_low, [on , off, on , on ]},
            {gts4,     [on , on , on , off]},
            {gts4_low, [on , on , on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_1, MC, oe_gts_mux1),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, GTS_1, OE],
           [{on,       [off, off, off, on ]},
            {gts1,     [on , off, off, off]},
            {gts1_low, [on , off, off, on ]},
            {gts2,     [on , on , off, off]},
            {gts2_low, [on , on , off, on ]},
            {gts4,     [on , on , on , off]},
            {gts4_low, [on , on , on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_1, MC, oe_gts_mux1),
     fuse(GTS_0, MC, oe_gts_mux0)
    ];
fuses(MC, {matrix,
           [GTS, GTS_0, GTS_1, OE],
           [{on,       [off, off, off, on ]},
            {gts1,     [on , off, off, off]},
            {gts1_low, [on , off, off, on ]},
            {gts2,     [on , on , off, off]},
            {gts2_low, [on , on , off, on ]},
            {gts3,     [on , off, on , off]},
            {gts3_low, [on , off, on , on ]}
           ]
          }) ->
    [fuse(OE, MC, oe_invert),
     fuse(GTS, MC, oe_mux),
     fuse(GTS_1, MC, oe_gts_mux1),
     fuse(GTS_0, MC, oe_gts_mux0)
    ].

%%--------------------------------------------------------------------

fuse(Fuse, MC, Extra) ->
    {Fuse, fuse:macro_cell(MC, Extra)}.

