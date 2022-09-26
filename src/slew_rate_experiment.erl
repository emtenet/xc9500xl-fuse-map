-module(slew_rate_experiment).

-export([run/0]).

%% Detect slew-rate fuse
%%
%% The slew-rate fuse turns an IO block from SLOW to FAST.
%%
%% Sweep through IO blocks (macro cells with an IO block) comparing
%% the fuses between SLOW and FAST. Sweep through in pairs so that
%% one is the input and the other is the output. Duplicate the last
%% IO block to the front so that the first & last both participate
%% in two pairs.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()),
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    io:format(" => slew-rate ~s~n", [Density]),
    Device = density:largest_device(Density),
    IOs = io_pairs(Device),
    Updates = run(IOs, Device, []),
    io:format("~n", []),
    fuses:update(Density, Updates).

%%--------------------------------------------------------------------

run([_], _, Updates) ->
    lists:reverse(Updates);
run([Input | IOs = [Output | _]], Device, Updates) ->
    Fast = experiment(Device, Output, Input, fast),
    Slow = experiment(Device, Output, Input, slow),
    % turn on FUSE to go from SLOW to FAST
    {[Fuse], []} = fuses:diff(Slow, Fast),
    Update = {Fuse, fuse:macro_cell(Output, fast)},
    run(IOs, Device, [Update | Updates]).

%%--------------------------------------------------------------------

io_pairs(Device) ->
    IOs = lists:sort([ IO || {_Pin, IO} <- device:io_pins(Device) ]),
    [lists:last(IOs) | IOs].

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, Output, Input, Slew) ->
    io:format(".", []),
    {UCF, VHDL} = experiment:compile([
        {input, Input},
        {output, Output, input}
    ]),
    Cache = experiment:cache(#{
        device => Device,
        slew => Slew,
        ucf => UCF,
        vhdl => VHDL
    }),
    experiment:cached_jed(Cache).

