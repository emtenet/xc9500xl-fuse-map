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
        {"bypass  d", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, t},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"bypass ~d", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"bypass  d XOR ( t AND  s)", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, <<"d XOR (t AND s)">>},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"bypass  d XOR (~t AND  s)", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, <<"d XOR ((NOT t) AND s)">>},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"bypass  d XOR ( t AND ~s)", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, <<"d XOR (t AND (NOT s))">>},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, t, #{clk => clk}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, t}, #{clk => clk}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d SET  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, s => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d SET ~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, s => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d RESET  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, r => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d RESET ~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, r => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK ~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"~d CLOCK  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"~d CLOCK ~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK  t OE  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => t, oe => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK ~t OE  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => {low, t}, oe => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK  t OE ~s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => t, oe => {low, s}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CLOCK ~t OE ~s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => {low, t}, oe => {low, s}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"~d CLOCK  t OE  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => t, oe => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {"~d CLOCK  t OE ~s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, {low, d}, #{clk => t, oe => {low, s}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE ~t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE  t SET  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t, s => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE ~t SET  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}, s => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE  t RESET  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => t, r => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d CE ~t RESET  s", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, ce => {low, t}, r => s}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d OE  t", [
            {clk, GCK, #{global => gck}},
            {d, D},
            {s, S},
            {t, T},
            {o, O, d, #{clk => clk, oe => t}},
            {z, Z, d, #{clk => clk, s => s}}
        ]},
        {" d OE ~t", [
            {clk, GCK, #{global => gck}},
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

