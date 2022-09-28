-module(compile_playground).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Density = xc9536xl,
    io:format(" => compile playground ~s~n", [Density]),
    Device = density:largest_device(Density),
    experiment(Device),
    Fuses = experiment:jed(),
    IMUX = experiment:imux(),
    Names = fuse_map:fuses(Density, Fuses),
    io:format("~p~n", [Names]),
    io:format("~p~n", [IMUX]).

%%====================================================================
%% experiment
%%====================================================================

experiment(Device) ->
    {UCF, VHDL} = experiment:compile([
        {input, mc01_02},
        {clock, mc01_03, #{global => gck}},
        {hidden, mc01_02, input, internal, #{clk => clock}},
        {output, mc01_01, hidden, #{clk => clock}}
    ]),
    experiment:run(#{
        device => Device,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }).

