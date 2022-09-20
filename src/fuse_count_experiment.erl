-module(fuse_count_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    output([run(Device) || Device <- device:list()]),
    ok.

%%--------------------------------------------------------------------

run(Device) ->
    io:format(" => fuse-count ~s~n", [Device]),
    experiment:run(#{
        device => Device,
        ucf => <<>>,
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "\n"
            "entity experiment is\n"
            "  Port ( output : out STD_LOGIC );\n"
            "end experiment;\n"
            "\n"
            "architecture Behavioral of experiment is begin\n"
            "  output <= '1';\n"
            "end Behavioral;\n"
        >>
    }),
    {Device, experiment:fuse_count()}.

%%====================================================================
%% output
%%====================================================================

output([]) ->
    ok;
output([{Device, Count} | Devices]) ->
    io:format("fuse_count(~s) -> ~p;~n", [Device, Count]),
    output(Devices).

