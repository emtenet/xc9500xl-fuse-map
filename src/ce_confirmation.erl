-module(ce_confirmation).

-export([run/0]).

%%  Confirm CE selection fuses.
%%
%%  CE selection:
%%
%%      ce_or_r ce_or_s -> CE selected
%%        0       0        always enabled
%%        1       0        pt4
%%        0       1        pt1
%%
%%  Product term 1 allocation:
%%
%%      pt1_mux1 pt1_mux0 -> pt1 allocation
%%        0        0         unused
%%        1        1         CE or SET
%%
%%  Product term 4 allocation:
%%
%%      pt4_mux1 pt4_mux0 -> pt4 allocation
%%        0        0         unused
%%        1        1         CE or RESET

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
    {Output, _} = experiment:pick(IOs, [GCK, GSR]),
    mc(Device, GCK, GSR, Output, IOs).

%%--------------------------------------------------------------------

mc(Device, GCK, GSR, Output, IOs) ->
    Used = [GCK, GSR, Output],
    {[Input, CE], _} = experiment:pick(2, IOs, Used),
    Sources = lists:flatten([
        off(GCK, Output, Input),
        pt1(CE, GCK, GSR, Output, Input),
        pt4(CE, GCK, GSR, Output, Input),
        auto(CE, GCK, Output, Input)
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

pt1(CE, GCK, GSR, Output, Input) ->
    {pt1, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {ce, CE},
        {r, GSR},
        {o, Output, i, #{clk => clk, ce => ce, r => r}}
    ]}.

%%--------------------------------------------------------------------

pt4(CE, GCK, GSR, Output, Input) ->
    {pt4, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {ce, CE},
        {s, GSR},
        {o, Output, i, #{clk => clk, ce => ce, s => s}}
    ]}.

%%--------------------------------------------------------------------

auto(CE, GCK, Output, Input) ->
    {auto, [
        {i, Input},
        {clk, GCK, #{global => gck}},
        {ce, CE},
        {o, Output, i, #{clk => clk, ce => ce}}
    ]}.

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

filter({_, _, pt4_mux0}) -> true;
filter({_, _, pt4_mux1}) -> true;
%
filter({_, _, pt1_mux0}) -> true;
filter({_, _, pt1_mux1}) -> true;
%
filter({_, _, ce_or_r}) -> true;
filter({_, _, ce_or_s}) -> true;
%
filter(_) -> false.

%%--------------------------------------------------------------------

expect([{FB, MC, pt4_mux0},
        {FB, MC, pt4_mux1},
        %
        {FB, MC, pt1_mux0},
        {FB, MC, pt1_mux1},
        %
        {FB, MC, ce_or_r},
        {FB, MC, ce_or_s}
       ],
       {matrix,
        [_, _, _, _, _, _],
        [{off,  [off, off,   off, off,   off, off]},
         {pt1,  [on,  on,    on,  on,    off, on ]},
         {pt4,  [on,  on,    on,  on,    on,  off]},
         {auto, [off, off,   on,  on,    off, on ]}
        ]
       }) ->
    ok.

