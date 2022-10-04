-module(inputs_experiment).

-export([run/0]).

%%  Detect mapping of input MUX bit fuses to input source.
%%
%%  Save mapping in the inputs:update/0 database

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()),
    %run(xc9572xl),
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    lists:foreach(fun (IO) -> run(Density, Device, IOs, IO) end, IOs),
    %[IO | _] = IOs,
    %run(Density, Device, IOs, IO),
    ok.

%%--------------------------------------------------------------------

run(Density, Device, IOs, Output) ->
    io:format(" => inputs ~s ~s~n", [Device, Output]),
    Inputs = [
        experiment(Density, Device, Input, Output)
        ||
        Input <- IOs,
        Input =/= Output
    ],
    io:format("~n", []),
    %
    %io:format("~p~n", [Inputs]),
    inputs:update(Density, Inputs),
    ok.

%%====================================================================
%% experiment
%%====================================================================

experiment(Density, Device, Input, Output) ->
    {UCF, VHDL} = experiment:compile([
        {i, Input},
        {o, Output, i}
    ]),
    io:format(".", []),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    Fuses = experiment:cached_jed(Cache),
    IMUX = experiment:cached_imux(Cache),
    FB = macro_cell:function_block(Output),
    #{FB := #{input := #{Input := Index}}} = IMUX,
    Inputs = fuse_map:inputs(Density, Fuses),
    #{FB := #{Index := MUX}} = Inputs,
    {FB, Index, MUX, {Input, input}}.

