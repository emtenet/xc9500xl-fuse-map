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
    Answers = run(IOs, Device, []),
    io:format("~n", []),
    fuses:update(Density, Answers).

%%--------------------------------------------------------------------

run([_], _, Answers) ->
    lists:reverse(Answers);
run([Input | IOs = [Output | _]], Device, Answers) ->
    Fast = experiment(Device, Output, Input, fast),
    Slow = experiment(Device, Output, Input, slow),
    % turn on FUSE to go from SLOW to FAST
    {[Fuse], []} = fuses:diff(Slow, Fast),
    Name = list_to_atom(io_lib:format("~s_slew_rate", [Output])),
    Answer = {Name, Fuse},
    run(IOs, Device, [Answer | Answers]).

%%--------------------------------------------------------------------

io_pairs(Device) ->
    IOs = lists:sort([ IO || {_Pin, IO} <- device:io_pins(Device) ]),
    [lists:last(IOs) | IOs].

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, Output, Input, Slew) ->
    io:format(".", []),
    Fuses = experiment:cache(#{
        device => Device,
        slew => Slew,
        ucf => <<
            "NET \"input\" LOC = \"", (macro_cell:name(Input))/binary, "\";\n"
            "NET \"output\" LOC = \"", (macro_cell:name(Output))/binary, "\";\n"
        >>,
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
    Fuses.

