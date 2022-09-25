-module(product_term_experiment).

-export([run/0]).

% Of the 5 product terms, which is used by default in bypass mode and
% for D of the flip-flop.
%
% Send a test signal (and it's complement) to each of the flip-flop
% functions as outlined in Figure 8 of the XC9500XL Familty Data Sheet.
%
% For example the CE can come from product terms 1 or 4, those are also
% used for the set (pt 1) and reset (pt 4). So if we use set & ce at the same
% time then the ce will have to use pt 4!
%
% ANSWER:
% The same product term is used by bypass, ff-d and clock. So the default
% product term is number 3.

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
    fuses:print(fuses:matrix(experiments(Device, [
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
        {clock_inv, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {clock_not_inv, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => {low, t}}},
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
        ]},
        {oe, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, oe => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {oe_not, [
            {clk, GCK},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, oe => {low, t}}},
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

