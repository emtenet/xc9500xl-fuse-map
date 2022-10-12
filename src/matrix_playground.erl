-module(matrix_playground).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Device = xc9536xl_cs48,
    [GCK | _] = device:gck_macro_cells(Device),
    D = mc02_01, % data
    T = mc02_02, % test input
    S = mc02_03, % set/reset
    O = mc01_01, % MC under test
    Z = mc01_02, % control ff
    matrix:print(matrix:diff(experiments(Device, [
        {bypass, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, t},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {bypass_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {bypass_xor, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, <<"d XOR t">>},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {bypass_xor_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, <<"d XOR (NOT t)">>},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {ff, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, t, #{clk => clk}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {ff_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, t}, #{clk => clk}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {set, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, s => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {set_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, s => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {reset, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, r => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {reset_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, r => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {clock, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {clock_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {ce, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {ce_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {set_ce, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t, s => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {set_ce_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}, s => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {reset_ce, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t, r => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {reset_ce_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}, r => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]}
    ]))).

%%====================================================================
%% experiment
%%====================================================================

experiments(Device, Experiments) ->
    [ experiment(Device, Experiment) || Experiment <- Experiments ].

%%--------------------------------------------------------------------

experiment(Device, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => matrix ~s ~s~n", [Device, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

