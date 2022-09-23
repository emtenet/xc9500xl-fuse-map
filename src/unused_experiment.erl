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
    Name = list_to_atom(io_lib:format("~s_ground", [MC])),
    Update = {Fuse, Name},
    unused(Es, Unused, [Update | Updates]).

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, Output, Input, Unused) ->
    io:format(".", []),
    %io:format(" => unused ~s ~s = ~s (~s)~n", [Device, Output, Input, Unused]),
    Fuses = experiment:cache(#{
        device => Device,
        ucf => <<
            "NET \"input\" LOC = \"", (macro_cell:name(Input))/binary, "\";\n"
            "NET \"output\" LOC = \"", (macro_cell:name(Output))/binary, "\";\n"
        >>,
        unused => Unused,
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    input : in  STD_LOGIC;\n"
            "    output : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is begin\n"
            "  output <= input;\n"
            "end behavioral;\n"
        >>
    }),
    Name = iolist_to_binary(io_lib:format(
        "~s <= ~s, ~s",
        [Output, Input, Unused]
    )),
    {Name, Fuses}.

