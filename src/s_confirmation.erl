-module(s_confirmation).

-export([run/0]).

%%  Confirm glip-flop set selection fuses.
%%
%%  The s_gsr fuse selection either:
%%
%%      s_gsr -> SET selection
%%        0      product term 1
%%        1      GSR
%%
%%  NOTE: To use product term 1, the ce_or_s fuse must be "off".
%%
%%  The global GSR signal can be inverted with the gsr_invert fuse.
%%
%%  Product term 1 allocation:
%%
%%      pt1_mux1 pt1_mux0 -> pt1 allocation
%%        1        1         CE or SET
%%        0        0         unused

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
    GSR = device:gsr_macro_cell(Device),
    {Output, _} = experiment:pick(IOs, [GSR, GCK]),
    mc(Device, Output, GSR, GCK, IOs).

%%--------------------------------------------------------------------

mc(Device, Output, GSR, GCK, IOs) ->
    Used = [Output, GSR, GCK],
    {Input, _} = experiment:pick(IOs, Used),
    Sources = lists:flatten([
        off(GCK, Output, Input),
        product_term(GSR, GCK, Output, Input),
        global(GSR, GCK, Output, Input)
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

product_term(GSR, GCK, Output, Input) ->
    [{pt1, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {s, GSR},
        {o, Output, i, #{clk => clk, s => s}}
     ]},
     {pt1_invert, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {s, GSR},
        {o, Output, i, #{clk => clk, s => {low, s}}}
     ]}
    ].

%%--------------------------------------------------------------------

global(GSR, GCK, Output, Input) ->
    [{gsr, [
        {i, Input},
        {gcl, GCK, #{global => gck}},
        {s, GSR, #{global => gsr}},
        {o, Output, i, #{clk => gcl, s => s}}
     ]},
     {gsr_invert, [
        {i, Input},
        {gcl, GCK, #{global => gck}},
        {s, GSR, #{global => gsr}},
        {o, Output, i, #{clk => gcl, s => {low, s}}}
     ]}
    ].

%%--------------------------------------------------------------------

experiment(Device, Output, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => set ~s ~s ~s~n", [Device, Output, Name]),
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

filter(gsr_invert) -> true;
%
filter({_, _, pt1_mux0}) -> true;
filter({_, _, pt1_mux1}) -> true;
%
filter({_, _, s_gsr}) -> true;
%
filter(_) -> false.

%%--------------------------------------------------------------------

expect([gsr_invert,
        %
        {FB, MC, pt1_mux0},
        {FB, MC, pt1_mux1},
        %
        {FB, MC, s_gsr}
       ],
       {matrix,
        [_, _, _, _],
        [{off,        [off,   off, off,   off]},
         {pt1,        [off,   on,  on,    off]},
         {pt1_invert, [off,   on,  on,    off]},
         {gsr,        [off,   off, off,   on ]},
         {gsr_invert, [on,    off, off,   on ]}
        ]
       }) ->
    ok.

