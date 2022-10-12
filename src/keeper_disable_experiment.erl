-module(keeper_disable_experiment).

-export([run/0]).

%%  Detect keeper-disable fuse at the device level.

%%====================================================================
%% run
%%====================================================================

run() ->
    [ run(Density) || Density <- density:list() ],
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    IOs = experiment:shuffle(device:io_macro_cells(Device)),
    {[I, O], _} = experiment:pick(2, IOs, []),
    Answers = [
        experiment(Device, I, O, float),
        experiment(Device, I, O, keeper)
    ],
    Matrix = matrix:diff(Answers),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    fuses(Names, Matrix).

%%--------------------------------------------------------------------

experiment(Device, I, O, Keeper) ->
    {UCF, VHDL} = experiment:compile([
        {i, I},
        {o, O, i}
    ]),
    io:format(" => keeper-disable ~s ~s->~s ~s~n", [Device, I, O, Keeper]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        terminate => Keeper,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Keeper, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

fuses([keeper_disable],
      {matrix,
       [_Fuse],
       [{float,  [on ]},
        {keeper, [off]}
       ]
      }) ->
    ok.

