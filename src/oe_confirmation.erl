-module(oe_confirmation).

-export([run/0]).

%%  Confirm OE selection fuses.
%%
%%  OE selection:
%%
%%      oe_mux -> OE selection
%%        0       product term 5 (or '0' if not allocated)
%%        1       one of the GTS
%%
%%  GTS selection:
%%
%%      oe_gts_mux1 oe_gts_mux0 -> GTS selected
%%        0           0            gts1
%%        1           0            gts2
%%        0           1            gts3
%%        1           1            gts4
%%
%%  When OE is permanently enabled the oe_mux, oe_gts_mux0 & oe_gts_mux1
%%  fuses are off and the oe_invert fuse is on.
%%
%%  Product term 5 selection:
%%
%%      pt5_mux1 pt5_mux0 -> pt5 allocation
%%        0        0         unused
%%        1        1         OE

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
    [GCK | _] = device:gck_macro_cells(Device),
    GTSs = device:gts_macro_cells(Device),
    {Output, _} = experiment:pick(IOs, [GCK | GTSs]),
    mc(Device, GCK, Output, IOs, GTSs).

%%--------------------------------------------------------------------

mc(Device, GCK, Output, IOs, GTSs) ->
    Used = [GCK, Output | GTSs],
    {Input, _} = experiment:pick(IOs, Used),
    Sources = lists:flatten([
        off(GCK, Output, Input),
        pt5(GTSs, GCK, Output, Input)
        |
        gtss(GTSs, [gts1, gts2, gts3, gts4], GCK, Output, Input, [])
    ]),
    Answers = [ experiment(Device, Output, Source) || Source <- Sources ],
    Matrix0 = matrix:all(Answers),
    Matrix = matrix:filter_by_name(fun filter/1, Device, Matrix0),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    expect(Names, Matrix).

%%--------------------------------------------------------------------

off(GCK, Output, Input) ->
    {off, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {o, Output, i, #{clk => clk}}
    ]}.

%%--------------------------------------------------------------------

pt5([GTS | _], GCK, Output, Input) ->
    [{pt5, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {oe, GTS},
        {o, Output, i, #{clk => clk, oe => oe}}
     ]},
     {pt5_invert, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {oe, GTS},
        {o, Output, i, #{clk => clk, oe => {low, oe}}}
     ]}
    ].

%%--------------------------------------------------------------------

gtss([], _, _, _, _, Sources) ->
    lists:reverse(Sources);
gtss([GTS | GTSs], [Name | Names], GCK, Output, Input, Sources) ->
    Source = [
        {Name, [
            {i, Input},
            {gck, GCK, #{global => gck}},
            {Name, GTS, #{global => gts}},
            {o, Output, i, #{clk => gck, oe => Name}}
        ]},
        {gts_invert(Name), [
            {i, Input},
            {gck, GCK, #{global => gck}},
            {Name, GTS, #{global => gts}},
            {o, Output, i, #{clk => gck, oe => {low, Name}}}
        ]}
    ],
    gtss(GTSs, Names, GCK, Output, Input, [Source | Sources]).

%%--------------------------------------------------------------------

gts_invert(Name) ->
    list_to_atom(lists:flatten(io_lib:format("~p_invert", [Name]))).

%%--------------------------------------------------------------------

experiment(Device, Output, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => oe ~s ~s ~s~n", [Device, Output, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        power => low,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

filter(gts1_enable) -> true;
filter(gts2_enable) -> true;
filter(gts3_enable) -> true;
filter(gts4_enable) -> true;
%
filter({_, _, pt5_mux0}) -> true;
filter({_, _, pt5_mux1}) -> true;
%
filter({_, _, oe_mux}) -> true;
filter({_, _, oe_gts_mux0}) -> true;
filter({_, _, oe_gts_mux1}) -> true;
filter({_, _, oe_invert}) -> true;
%
filter(_) -> false.

%%--------------------------------------------------------------------

expect([gts1_enable,
        gts2_enable,
        %
        {FB, MC, pt5_mux0},
        {FB, MC, pt5_mux1},
        %
        {FB, MC, oe_mux},
        {FB, MC, oe_gts_mux0},
        {FB, MC, oe_invert}
       ],
       {matrix,
        [_, _, _, _, _, _, _],
        [{off,         [off, off,   off, off,   off, off, on ]},
         {pt5,         [off, off,   on,  on,    off, off, off]},
         {pt5_invert,  [off, off,   on,  on,    off, off, off]},
         {gts1,        [on,  off,   off, off,   on,  off, off]},
         {gts1_invert, [on,  off,   off, off,   on,  off, on ]},
         {gts2,        [off, on,    off, off,   on,  on,  off]},
         {gts2_invert, [off, on,    off, off,   on,  on,  on ]}
        ]
       }) ->
    ok;
expect([gts1_enable,
        gts2_enable,
        gts3_enable,
        gts4_enable,
        %
        {FB, MC, pt5_mux0},
        {FB, MC, pt5_mux1},
        %
        {FB, MC, oe_mux},
        {FB, MC, oe_gts_mux0},
        {FB, MC, oe_gts_mux1},
        {FB, MC, oe_invert}
       ],
       {matrix,
        [_, _, _, _, _, _, _, _, _, _],
        [{off,         [off, off, off, off,   off, off,   off, off, off, on ]},
         {pt5,         [off, off, off, off,   on,  on,    off, off, off, off]},
         {pt5_invert,  [off, off, off, off,   on,  on,    off, off, off, off]},
         {gts1,        [on,  off, off, off,   off, off,   on,  off, off, off]},
         {gts1_invert, [on,  off, off, off,   off, off,   on,  off, off, on ]},
         {gts2,        [off, on,  off, off,   off, off,   on,  on,  off, off]},
         {gts2_invert, [off, on,  off, off,   off, off,   on,  on,  off, on ]},
         {gts3,        [off, off, on,  off,   off, off,   on,  off, on,  off]},
         {gts3_invert, [off, off, on,  off,   off, off,   on,  off, on,  on ]},
         {gts4,        [off, off, off, on,    off, off,   on,  on,  on,  off]},
         {gts4_invert, [off, off, off, on,    off, off,   on,  on,  on,  on ]}
        ]
       }) ->
    ok.

