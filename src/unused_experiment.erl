-module(unused_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()),
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    io:format(" => unused ~s~n", [Density]),
    Device = density:largest_device(Density),
    MC_count = density:macro_cell_count(Density),
    IOs0 = lists:sort([ IO || {_Pin, IO} <- device:io_pins(Device) ]),
    IO_count = length(IOs0),
    IOs = [lists:last(IOs0) | IOs0],
    Experiments = experiments(Device, IOs, []),
    {Floats, Grounds} = union(Experiments, [], []),
    {Unused, []} = fuses:diff(Floats, Grounds),
    MC_count = length(Unused),
    Update = unused([lists:last(Experiments) | Experiments], Unused, []),
    IO_count = length(Update),
    fuses:update(Density, Update),
    ok.

%%--------------------------------------------------------------------

experiments(_, [_], Experiments) ->
    io:format("~n", []),
    lists:reverse(Experiments);
experiments(Device, [Output | IOs = [Input | _]], Experiments) ->
    Float = experiment(Device, Output, Input, float),
    Ground = experiment(Device, Output, Input, ground),
    Experiment = {Input, Float, Ground},
    experiments(Device, IOs, [Experiment | Experiments]).

%%--------------------------------------------------------------------

union([], Floats, Grounds) ->
    {Floats, Grounds};
union([{_, {_, Float}, {_, Ground}} | Experiments], Floats0, Grounds0) ->
    Floats = fuses:union(Float, Floats0),
    Grounds = fuses:union(Ground, Grounds0),
    union(Experiments, Floats, Grounds).

%%--------------------------------------------------------------------

unused([_], _, Updates) ->
    lists:reverse(Updates);
unused([{MC, _, {_, G1}} | Es = [{_, _, {_, G2}} | _]], Unused, Updates) ->
    Grounds = fuses:union(G1, G2),
    [Fuse] = fuses:subtract(Unused, Grounds),
    Update = {Fuse, fuse:macro_cell(MC, ground)},
    unused(Es, Unused, [Update | Updates]).

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, Output, Input, Unused) ->
    io:format(".", []),
    {UCF, VHDL} = experiment:compile([
        {input, Input},
        {output, Output, input}
    ]),
    Cache = experiment:cache(#{
        device => Device,
        ucf => UCF,
        unused => Unused,
        vhdl => VHDL
    }),
    Name = iolist_to_binary(io_lib:format(
        "~s <= ~s, ~s",
        [Output, Input, Unused]
    )),
    {Name, experiment:cached_jed(Cache)}.

