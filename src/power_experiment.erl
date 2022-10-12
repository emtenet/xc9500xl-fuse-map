-module(power_experiment).

-export([run/0]).

%%  Detect power fuses per macro-cell.
%%
%%  NOTE: There are two fuses that always appear to be on together.
%%  So they have been named power_a & power_b.

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
        experiment(Device, I, O, low),
        experiment(Device, I, O, std)
    ],
    Matrix = matrix:diff(Answers),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    fuses(Names, Matrix).

%%--------------------------------------------------------------------

experiment(Device, I, O, Power) ->
    {UCF, VHDL} = experiment:compile([
        {i, I},
        {o, O, i, #{power => Power}}
    ]),
    io:format(" => power ~s ~s->~s ~s~n", [Device, I, O, Power]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        power => low,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Power, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

fuses([{FB, MC, power_a}, {FB, MC, power_b}],
      {matrix,
       [_, _],
       [{low, [off, off]},
        {std, [on,  on ]}
       ]
      }) ->
    ok.

