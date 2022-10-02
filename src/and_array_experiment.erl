-module(and_array_experiment).

-export([run/0]).
-export([run/1]).
-export([run/2]).

%%  Detect some and-array fuses from a single i/o.
%%
%%  The and-array fuses will be for product-term #3 as determined by
%%  the product_term_experiment.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    density(Density),
    ok.

%%--------------------------------------------------------------------

run(Density, MacroCell) ->
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    true = lists:member(MacroCell, IOs),
    macro_cell(Device, IOs, MacroCell),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    lists:foreach(fun (IO) -> macro_cell(Device, IOs, IO) end, IOs),
    ok.

%%--------------------------------------------------------------------

macro_cell(Device, IOs, Output) ->
    io:format(" => fast-connect ~s ~s~n", [Device, Output]),
    Results = [
        experiment(Device, Input, Output)
        ||
        Input <- IOs,
        Input =/= Output
    ],
    io:format("~n", []),
    %
    Fuses = lists:flatten([ fuses(Result) || Result <- Results ]),
    %io:format("~p~n", [Fuses]),
    fuses:update(Device, Fuses).

%%--------------------------------------------------------------------

fuses({_Input, Output, Direct, Invert, Index}) ->
    {[InvertFuse], [DirectFuse]} = fuses:diff(Direct, Invert),
    [
        {DirectFuse, fuse:and_array(Output, pt3, Index)},
        {InvertFuse, fuse:and_array(Output, pt3, Index, invert)}
    ].

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, Input, Output) ->
    {Direct, Index} = experiment(Device, Input, Output, direct),
    {Invert, Index} = experiment(Device, Input, Output, invert),
    {Input, Output, Direct, Invert, Index}.

%%--------------------------------------------------------------------

experiment(Device, Input, Output, Invert) ->
    Logic = case Invert of
        direct -> i;
        invert -> {low, i}
    end,
    {UCF, VHDL} = experiment:compile([
        {i, Input},
        {o, Output, Logic}
    ]),
    io:format(".", []),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    JED = experiment:cached_jed(Cache),
    IMUX = experiment:cached_imux(Cache),
    FB = macro_cell:function_block(Output),
    #{FB := #{input := #{Input := Index}}} = IMUX,
    {JED, Index}.

