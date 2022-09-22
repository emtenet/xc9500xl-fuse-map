-module(names_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Density = xc9536xl,
    io:format(" => names ~s~n", [Density]),
    Device = density:largest_device(Density),
    Fuses = experiment(Device),
    Database = fuses:read(Density),
    fuses:name_if_known(Fuses, Database).

%%====================================================================
%% experiment
%%====================================================================

experiment(Device) ->
    experiment:run(#{
        device => Device,
        slew => fast,
        ucf => <<
            "NET \"input\" LOC = \"FB2_1\";\n"
            "NET \"output\" LOC = \"FB1_1\";\n"
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
    experiment:jed().

