-module(vhdl_playground).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Density = xc9536xl,
    io:format(" => VHDL playground ~s~n", [Density]),
    Device = density:largest_device(Density),
    experiment(Device),
    Fuses = experiment:jed(),
    IMUX = experiment:imux(),
    Names = fuse_map:fuses(Density, Fuses),
    Inputs = fuse_map:inputs(Density, Fuses),
    io:format("~p~n", [Names]),
    io:format("~p~n", [IMUX]),
    io:format("~p~n", [Inputs]).

%%====================================================================
%% experiment
%%====================================================================

experiment(Device) ->
    experiment:run(#{
        device => Device,
        slew => slow,
        ucf => <<
            "NET \"input\" LOC = \"FB1_2\";\n"
            "NET \"clock\" LOC = \"FB1_3\" | BUFG = CLK;\n"
            "NET \"output\" LOC = \"FB1_1\";\n"
            "NET \"hidden\" LOC = \"FB1_2\";\n"
        >>,
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library UNISIM;\n"
            "use UNISIM.vcomponents.ALL;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    input : in STD_LOGIC;\n"
            "    clock : in STD_LOGIC;\n"
            "    output : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "  signal hidden : STD_LOGIC;\n"
            "begin\n"
            "  hidden_FF: FD generic map ('0') port map (\n"
            "    D => NOT input,\n"
            "    Q => hidden,\n"
            "    C => clock\n"
            "  );\n"
            "  output_FF: FD generic map ('0') port map (\n"
            "    D => hidden,\n"
            "    Q => output,\n"
            "    C => clock\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }).

