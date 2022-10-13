-module(power_experiment).

-export([run/0]).

%%  Detect power fuses per macro-cell.
%%
%%  The power fuses enable STD-power, i.e. low-power in absense.
%%
%%  Each of the five product terms has a power fuse.
%%
%%  There is also a power fuse for the "combinatorial" logic whether
%%  that is bypassed or directed to the flip-flop.
%%
%%  So if a macro cell is forwarding product terms and not using it's
%%  flio-flop or producing an output, then the "combinatorial" fuse
%%  is not needed, only the power fuse for each product term that is
%%  being forwarded.
%%
%%  NOTE: Initially the secondary product term (non pt3) power fuses
%%  where confused for "enable" fuses. But that was because the power
%%  experiments where only using product term 3.

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
    bypass(Device, IOs),
    bypass_oe(Device, IOs),
    bypass_xor(Device, IOs),
    flip_flop(Device, IOs),
    flip_flop_set(Device, IOs),
    flip_flop_reset(Device, IOs),
    flip_flop_oe_reset(Device, IOs),
    flip_flop_oe_set_reset(Device, IOs),
    forward(Device, IOs).

%%--------------------------------------------------------------------

bypass(Device, IOs) ->
    {[I, O], _} = experiment:pick(2, IOs, []),
    experiments(Device, "bypass", fun (Power) -> [
        {i, I},
        {o, O, i, #{power => Power}}
    ] end, [
        std_power,
        pt3_std_power
    ]).

%%--------------------------------------------------------------------

bypass_oe(Device, IOs) ->
    {[I, O, OE], _} = experiment:pick(3, IOs, []),
    experiments(Device, "bypass-oe", fun (Power) -> [
        {i, I},
        {oe, OE},
        {o, O, i, #{oe => oe, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power
    ]).

%%--------------------------------------------------------------------

bypass_xor(Device, IOs) ->
    {[I, O, X], _} = experiment:pick(3, IOs, []),
    experiments(Device, "bypass-xor", fun (Power) -> [
        {i, I},
        {x, X},
        {o, O, <<"i XOR x">>, #{power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt2_std_power
    ]).

%%--------------------------------------------------------------------

flip_flop(Device, IOs) ->
    {[I, O, CLK], _} = experiment:pick(3, IOs, []),
    experiments(Device, "flip-flop", fun (Power) -> [
        {i, I},
        {clk, CLK},
        {o, O, i, #{clk => clk, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power
    ]).

%%--------------------------------------------------------------------

flip_flop_set(Device, IOs) ->
    {[I, O, CLK, SET], _} = experiment:pick(4, IOs, []),
    experiments(Device, "flip-flop-set", fun (Power) -> [
        {i, I},
        {clk, CLK},
        {set, SET},
        {o, O, i, #{clk => clk, s => set, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power,
        pt1_std_power
    ]).

%%--------------------------------------------------------------------

flip_flop_reset(Device, IOs) ->
    {[I, O, CLK, RESET], _} = experiment:pick(4, IOs, []),
    experiments(Device, "flip-flop-reset", fun (Power) -> [
        {i, I},
        {clk, CLK},
        {reset, RESET},
        {o, O, i, #{clk => clk, r => reset, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power,
        pt4_std_power
    ]).

%%--------------------------------------------------------------------

flip_flop_oe_reset(Device, IOs) ->
    {[I, O, CLK, OE, RESET], _} = experiment:pick(5, IOs, []),
    experiments(Device, "flip-flop-oe-reset", fun (Power) -> [
        {i, I},
        {clk, CLK},
        {oe, OE},
        {reset, RESET},
        {o, O, i, #{clk => clk, oe => oe, r => reset, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power,
        pt4_std_power,
        pt1_std_power
    ]).

%%--------------------------------------------------------------------

flip_flop_oe_set_reset(Device, IOs) ->
    {[I, O, CLK, OE, SET, RESET], _} = experiment:pick(6, IOs, []),
    experiments(Device, "flip-flop-oe-set-reset", fun (Power) -> [
        {i, I},
        {clk, CLK},
        {oe, OE},
        {set, SET},
        {reset, RESET},
        {o, O, i, #{clk => clk, oe => oe, s => set, r => reset, power => Power}}
    ] end, [
        std_power,
        pt3_std_power,
        pt5_std_power,
        pt4_std_power,
        pt1_std_power,
        pt2_std_power
    ]).

%%--------------------------------------------------------------------

forward(Device, IOs) ->
    {[I, O, CLK, OE, SET, RESET, X], _} = experiment:pick(7, IOs, []),
    experiments(Device, "forward", fun (Power) -> [
        {i, I},
        {x, X},
        {clk, CLK},
        {oe, OE},
        {set, SET},
        {reset, RESET},
        {o, O, <<"i XOR x">>, #{clk => clk, oe => oe, s => set, r => reset, power => Power}}
    ] end, [
        std_power,
        pt3_std_power, pt3_std_power,
        pt5_std_power,
        pt4_std_power,
        pt1_std_power,
        pt2_std_power
    ]).

%%--------------------------------------------------------------------

experiments(Device, Name, Signals, Expect) ->
    Answers = [
        experiment(Device, Name, Signals, low),
        experiment(Device, Name, Signals, std)
    ],
    Matrix = matrix:diff(Answers),
    %matrix:print_names(Device, Matrix),
    %matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    fuses(Expect, Names, Matrix).

%%--------------------------------------------------------------------

experiment(Device, Name, Signals, Power) ->
    {UCF, VHDL} = experiment:compile(Signals(Power)),
    io:format(" => power ~s ~s ~s~n", [Device, Name, Power]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        power => low,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Power, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

fuses([A, B],
      [{FB, MC, A}, {FB, MC, B}],
      {matrix,
       [_, _],
       [{low, [off, off]},
        {std, [on,  on ]}
       ]
      }) ->
    ok;
fuses([A, B, C],
      [{FB, MC, A}, {FB, MC, B}, {FB, MC, C}],
      {matrix,
       [_, _, _],
       [{low, [off, off, off]},
        {std, [on,  on,  on ]}
       ]
      }) ->
    ok;
fuses([A, B, C, D],
      [{FB, MC, A}, {FB, MC, B}, {FB, MC, C}, {FB, MC, D}],
      {matrix,
       [_, _, _, _],
       [{low, [off, off, off, off]},
        {std, [on,  on,  on,  on ]}
       ]
      }) ->
    ok;
fuses([A, B, C, D, E],
      [{FB, MC, A}, {FB, MC, B}, {FB, MC, C}, {FB, MC, D}, {FB, MC, E}],
      {matrix,
       [_, _, _, _, _],
       [{low, [off, off, off, off, off]},
        {std, [on,  on,  on,  on,  on ]}
       ]
      }) ->
    ok;
fuses([A, B, C, D, E, F],
      [{FB, MC, A}, {FB, MC, B}, {FB, MC, C}, {FB, MC, D}, {FB, MC, E}, {FB, MC, F}],
      {matrix,
       [_, _, _, _, _, _],
       [{low, [off, off, off, off, off, off]},
        {std, [on,  on,  on,  on,  on,  on ]}
       ]
      }) ->
    ok;
fuses([A, B, B, C, D, E, F],
      [{FB, MC, A},
       {FB, _, B},
       {FB, _, B},
       {FB, MC, C},
       {FB, MC, D},
       {FB, MC, E},
       {FB, MC, F}
      ],
      {matrix,
       [_, _, _, _, _, _, _],
       [{low, [off, off, off, off, off, off, off]},
        {std, [on,  on,  on,  on,  on,  on,  on ]}
       ]
      }) ->
    ok.

