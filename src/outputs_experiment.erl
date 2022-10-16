-module(outputs_experiment).

-export([run/0]).

%%  Detect mapping of input MUX bit fuses to feedback (output) source.
%%
%%  Save mapping in the inputs:update/0 database

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()),
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    MCs = density:macro_cells(Device),
    GCKs = device:gck_macro_cells(Device),
    lists:foreach(fun (IO) ->
        run(Density, Device, MCs, IOs, GCKs, IO) end,
        IOs
    ),
    ok.

%%--------------------------------------------------------------------

run(Density, Device, MCs, IOs, GCKs, Output) ->
    io:format(" => outputs ~s ~s~n", [Device, Output]),
    Inputs = [
        experiment(Density, Device, GCKs, Internal, Output, IOs)
        ||
        Internal <- MCs,
        Internal =/= Output
    ],
    io:format("~n", []),
    inputs:update(Density, Inputs),
    ok.

%%====================================================================
%% experiment
%%====================================================================

experiment(Density, Device, GCKs, Internal, Output, IOs) ->
    {GCK, _} = experiment:pick(GCKs, [Internal, Output]),
    {Input, _} = experiment:pick(IOs, [GCK, Internal, Output]),
    {UCF, VHDL} = experiment:compile([
        {i, Input},
        {c, GCK, #{global => gck}},
        {q, Internal, i, internal, #{clk => c}},
        {o, Output, q, #{clk => c}}
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
    #{FB := #{output := #{Internal := Index}}} = IMUX,
    Inputs = fuse_map:inputs(Density, Fuses),
    #{FB := #{Index := MUX}} = Inputs,
    {FB, Index, MUX, {Internal, output}}.

