-module(r_confirmation).

-export([run/0]).

%%  Confirm glip-flop reset selection fuses.
%%
%%  The r_gsr fuse selection either:
%%
%%      r_gsr -> set source
%%        0      product term 4
%%        1      GSR
%%
%%  NOTE: To use product term 4, the ce_or_r fuse must be "off".
%%
%%  The global GSR signal can be inverted with the gsr_invert fuse.
%%
%%  Product term 4 allocation
%%
%%      pt4_mux1 pt4_mux0 -> pt4 allocation
%%        1        1         CE or RESET
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
    [{pt4, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {r, GSR},
        {o, Output, i, #{clk => clk, r => r}}
     ]},
     {pt4_invert, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {r, GSR},
        {o, Output, i, #{clk => clk, r => {low, r}}}
     ]}
    ].

%%--------------------------------------------------------------------

global(GSR, GCK, Output, Input) ->
    [{gsr, [
        {i, Input},
        {gcl, GCK, #{global => gck}},
        {r, GSR, #{global => gsr}},
        {o, Output, i, #{clk => gcl, r => r}}
     ]},
     {gsr_invert, [
        {i, Input},
        {gcl, GCK, #{global => gck}},
        {r, GSR, #{global => gsr}},
        {o, Output, i, #{clk => gcl, r => {low, r}}}
     ]}
    ].

%%--------------------------------------------------------------------

experiment(Device, Output, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => reset ~s ~s ~s~n", [Device, Output, Name]),
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
filter({_, _, pt4_mux0}) -> true;
filter({_, _, pt4_mux1}) -> true;
%
filter({_, _, r_gsr}) -> true;
%
filter(_) -> false.

%%--------------------------------------------------------------------

expect([gsr_invert,
        %
        {FB, MC, pt4_mux0},
        {FB, MC, pt4_mux1},
        %
        {FB, MC, r_gsr}
       ],
       {matrix,
        [_, _, _, _],
        [{off,        [off,   off, off,   off]},
         {pt4,        [off,   on,  on,    off]},
         {pt4_invert, [off,   on,  on,    off]},
         {gsr,        [off,   off, off,   on ]},
         {gsr_invert, [on,    off, off,   on ]}
        ]
       }) ->
    ok.

