-module(fast_connect_experiment).

-export([run/0]).

%%  Detect some fast-connect & and-array fuses from a single i/o.
%%
%%  The and-array fuses will be for product-term #3 as determined by
%%  the product_term_experiment.
%%
%%  Each MC input/output is available at three places within the fast
%%  connect matrix, this experiment will only detect one of those three.

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
    lists:foreach(fun (IO) -> run(Device, IOs, IO) end, IOs),
    ok.

%%--------------------------------------------------------------------

run(Device, IOs, Output) ->
    io:format(" => fast-connect ~s ~s~n", [Device, Output]),
    Results = [
        experiment(Device, Input, Output)
        ||
        Input <- IOs,
        Input =/= Output
    ],
    io:format("~n", []),
    Intersect = intersect(Results),
    %io:format("~p~n", [Intersect]),
    Fuses = lists:flatten([ reduce(Result, Intersect) || Result <- Results ]),
    %io:format("~p~n", [Fuses]),
    fuses:update(Device, Fuses).

%%--------------------------------------------------------------------

intersect(Results) ->
    lists:foldl(fun intersect/2, undefined, Results).

%%--------------------------------------------------------------------

intersect({_, _, Direct, Invert, _}, undefined) ->
    fuses:intersect(Direct, Invert);
intersect({_, _, Direct, Invert, _}, Acc0) ->
    Acc1 = fuses:intersect(Direct, Acc0),
    Acc2 = fuses:intersect(Invert, Acc1),
    Acc2.

%%--------------------------------------------------------------------

reduce({Input, Output, Direct, Invert, Index}, Intersect) ->
    {[InvertFuse], [DirectFuse]} = fuses:diff(Direct, Invert),
    [
        {DirectFuse, fuse:and_array(Output, pt3, Index)},
        {InvertFuse, fuse:and_array(Output, pt3, Index, invert)}
    ].
    %DirectDiff = fuses:subtract(Direct, Intersect),
    %InvertDiff = fuses:subtract(Invert, Intersect),
    %case {DirectDiff, InvertDiff} of
    %    {[Fuse, DirectFuse], [Fuse, InvertFuse]}
    %            when DirectFuse =/= InvertFuse ->
    %        reduce(Input, Output, Index, Fuse, DirectFuse, InvertFuse);

    %    {[DirectFuse, Fuse], [InvertFuse, Fuse]}
    %            when DirectFuse =/= InvertFuse ->
    %        reduce(Input, Output, Index, Fuse, DirectFuse, InvertFuse);

    %    {[DirectFuse, FuseA, FuseB], [InvertFuse, FuseA, FuseB]}
    %            when DirectFuse =/= InvertFuse ->
    %        io:format("SKIP ~s ~s ~s ~p ~p~n", [Input, Output, Index, DirectDiff, InvertDiff]),
    %        reduce(Input, Output, Index, undefined, DirectFuse, InvertFuse);

    %    {[FuseA, DirectFuse, FuseB], [FuseA, InvertFuse, FuseB]}
    %            when DirectFuse =/= InvertFuse ->
    %        io:format("SKIP ~s ~s ~s ~p ~p~n", [Input, Output, Index, DirectDiff, InvertDiff]),
    %        reduce(Input, Output, Index, undefined, DirectFuse, InvertFuse);

    %    {[FuseA, FuseB, DirectFuse], [FuseA, FuseB, InvertFuse]}
    %            when DirectFuse =/= InvertFuse ->
    %        io:format("SKIP ~s ~s ~s ~p ~p~n", [Input, Output, Index, DirectDiff, InvertDiff]),
    %        reduce(Input, Output, Index, undefined, DirectFuse, InvertFuse)
    %end.

%%--------------------------------------------------------------------

%reduce(_Input, Output, Index, undefined, DirectFuse, InvertFuse) ->
%    Fuses = [
%        {DirectFuse, fuse:and_array(Output, pt3, Index)},
%        {InvertFuse, fuse:and_array(Output, pt3, Index, invert)}
%    ],
%    Fuses;
%reduce(_Input, Output, Index, _Fuse, DirectFuse, InvertFuse) ->
%    %FB = macro_cell:function_block(Output),
%    Fuses = [
%        %{Fuse, fuse:fast_connect(FB, Index, input, Input)},
%        {DirectFuse, fuse:and_array(Output, pt3, Index)},
%        {InvertFuse, fuse:and_array(Output, pt3, Index, invert)}
%    ],
%    Fuses.

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

