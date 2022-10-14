-module(clk_confirmation).

-export([run/0]).

%%  Confirm clock selection fuses.
%%
%%  Clock selection:
%%
%%      clk_mux1 clk_mux0 -> clk selected
%%        0        0         gck2
%%        1        0         gck1
%%        0        1         gck3
%%        1        1         product term 3
%%
%%  The selected clock can be inverted with the clk_invert fuse. Although
%%  this is not seen in practive with the pt3 selection since the
%%  product term can self-invert in our experiments.
%%
%%  When flip-flop is bypassed and no clock is needed then both clock
%%  selection fuses are off.
%%
%%  Product term 3 selection:
%%
%%      pt3_mux1 pt3_mux0 -> pt3 used for
%%        1        1         clock
%%        0        1         bypass or flip-flop D signal

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
    GCKs = device:gck_macro_cells(Device),
    {Output, _} = experiment:pick(IOs, GCKs),
    mc(Device, Output, IOs, GCKs).

%%--------------------------------------------------------------------

mc(Device, Output, IOs, GCKs) ->
    Used = [Output | GCKs],
    {Input, _} = experiment:pick(IOs, Used),
    Sources = lists:flatten([
        off(Output, Input),
        pt3(GCKs, Output, Input)
        |
        gcks(GCKs, [gck1, gck2, gck3], Output, Input, [])
    ]),
    Answers = [ experiment(Device, Output, Source) || Source <- Sources ],
    Matrix0 = matrix:all(Answers),
    Matrix = matrix:filter_by_name(fun filter/1, Device, Matrix0),
    matrix:print_names(Device, Matrix),
    matrix:print(Matrix),
    Names = matrix:names(Device, Matrix),
    expect(Names, Matrix).

%%--------------------------------------------------------------------

off(Output, Input) ->
    {off, [
        {i, Input},
        {o, Output, i}
    ]}.

%%--------------------------------------------------------------------

pt3([GCK | _], Output, Input) ->
    [{pt3, [
        {i, Input},
        {clk, GCK},
        {o, Output, i, #{clk => clk}}
     ]},
     {pt3_invert, [
        {i, Input},
        {clk, GCK},
        {o, Output, i, #{clk => {low, clk}}}
     ]}
    ].

%%--------------------------------------------------------------------

gcks([], _, _, _, Sources) ->
    lists:reverse(Sources);
gcks([GCK | GCKs], [Name | Names], Output, Input, Sources) ->
    Source = [
        {Name, [
            {i, Input},
            {Name, GCK, #{global => gck}},
            {o, Output, i, #{clk => Name}}
        ]},
        {gck_invert(Name), [
            {i, Input},
            {Name, GCK, #{global => gck}},
            {o, Output, i, #{clk => {low, Name}}}
        ]}
    ],
    gcks(GCKs, Names, Output, Input, [Source | Sources]).

%%--------------------------------------------------------------------

gck_invert(Name) ->
    list_to_atom(lists:flatten(io_lib:format("~p_invert", [Name]))).

%%--------------------------------------------------------------------

experiment(Device, Output, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => clk ~s ~s ~s~n", [Device, Output, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        power => low,
        slew => slow,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

filter(gck1_enable) -> true;
filter(gck2_enable) -> true;
filter(gck3_enable) -> true;
%
filter({_, _, pt3_mux0}) -> true;
filter({_, _, pt3_mux1}) -> true;
%
filter({_, _, clk_mux0}) -> true;
filter({_, _, clk_mux1}) -> true;
filter({_, _, clk_invert}) -> true;
%
filter(_) -> false.

%%--------------------------------------------------------------------

expect([gck1_enable,
        gck2_enable,
        gck3_enable,
        %
        {FB, MC, pt3_mux0},
        {FB, MC, pt3_mux1},
        %
        {FB, MC, clk_mux0},
        {FB, MC, clk_mux1},
        {FB, MC, clk_invert}
       ],
       {matrix,
        [_, _, _, _, _, _, _, _],
        [{off,         [off, off, off,   on, off,   off, off, off]},
         {pt3,         [off, off, off,   on, on,    on,  on,  off]},
         {pt3_invert,  [off, off, off,   on, on,    on,  on,  off]},
         {gck1,        [on,  off, off,   on, off,   off, on,  off]},
         {gck1_invert, [on,  off, off,   on, off,   off, on,  on ]},
         {gck2,        [off, on,  off,   on, off,   off, off, off]},
         {gck2_invert, [off, on,  off,   on, off,   off, off, on ]},
         {gck3,        [off, off, on,    on, off,   on,  off, off]},
         {gck3_invert, [off, off, on,    on, off,   on,  off, on ]}
        ]
       }) ->
    ok.

