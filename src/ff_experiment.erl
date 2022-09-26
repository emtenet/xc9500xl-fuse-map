-module(ff_experiment).

-export([run/0]).

%%  Detect d/t-type flip-flop & initial fuse
%%
%%  Each macro-cell's flip-flop can be either d-type or t-type
%%
%%  Each macro-cell's flip-flop can be preset to '0' or '1'

%%====================================================================
%% run
%%====================================================================

run() ->
    [ run(Density) || Density <- density:list() ],
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    MCs = device:io_macro_cells(Device),
    GCKs = device:gck_macro_cells(Device),
    Fuses = lists:flatten([
        mc(Device, MC, MCs, gck(MC, GCKs))
        ||
        MC <- MCs
    ]),
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

gck(MC, [MC, GCK | _]) ->
    GCK;
gck(_, [GCK | _]) ->
    GCK.

%%--------------------------------------------------------------------

mc(Device, O, Avail0, GCK) ->
    Used = [O, GCK],
    {I, _} = unused(Avail0, Used),
    Sources = [
        source(d, I, GCK, O),
        source(t, I, GCK, O),
        source(init, I, GCK, O)
    ],
    Answers = [ experiment(Device, O, Source) || Source <- Sources ],
    Matrix = fuses:matrix(Answers),
    fuses:print(Matrix),
    fuses(O, Matrix).

%%--------------------------------------------------------------------

unused([IO | IOs], Used) ->
    case lists:member(IO, Used) of
        true ->
            unused(IOs, Used);

        false ->
            {IO, IOs}
    end.

%%--------------------------------------------------------------------

source(Name, I, GCK, O) ->
    FF = case Name of
        d -> #{clk => gck, type => d};
        t -> #{clk => gck, type => t};
        init -> #{clk => gck, init => 1}
    end,
    {Name, [
        {i, I},
        {gck, GCK, #{global => gck}},
        {o, O, i, FF}
    ]}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => ff ~s ~s ~s~n", [Device, MC, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------


fuses(MC, {matrix,
           [T, INIT],
           [{d,    [off, off]},
            {t,    [on,  off]},
            {init, [off, on ]}
           ]
          }) ->
    [fuse(T, MC, t_type),
     fuse(INIT, MC, preset)
    ].

%%--------------------------------------------------------------------

fuse(Fuse, MC, Name) ->
    {Fuse, fuse:macro_cell(MC, Name)}.

