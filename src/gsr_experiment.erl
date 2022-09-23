-module(gsr_experiment).

-export([run/0]).

%%  Detect GS (set) & GR (reset) fuses
%%
%%  Each macro-cell's flip-flop can be asynchronously set & reset either
%%  from a product-term or GSR (possibly inverted).
%%
%%  Detect the GS fuse when setting via GSR.
%%  Detect the GR fuse when resetting via GSR.
%%
%%  We do not attempt to detect the product-term fuses.

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
    GSR = device:gsr_macro_cell(Device),
    GCKs = device:gck_macro_cells(Device),
    Fuses = lists:flatten([
        mc(Device, MC, MCs, GSR, gck(MC, GCKs))
        ||
        MC <- MCs,
        MC =/= GSR
    ]),
    io:format("update ~p~n", [Fuses]),
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

gck(MC, [MC, GCK | _]) ->
    GCK;
gck(_, [GCK | _]) ->
    GCK.

%%--------------------------------------------------------------------

mc(Device, O, Avail0, GSR, GCK) ->
    Used = [O, GSR, GCK],
    {I, Avail1} = unused(Avail0, Used),
    {X, _} = unused(Avail1, Used),
    Sources = [
        source(off, I, GSR, GCK, O, X),
        source(s, I, GSR, GCK, O, X),
        source(r, I, GSR, GCK, O, X)
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

source(Name, I, GSR, GCK, O, X) ->
    FF = case Name of
        off -> #{clk => gck};
        s -> #{clk => gck, s => gsr};
        r -> #{clk => gck, r => gsr}
    end,
    {Name, [
        {i, I},
        {gck, GCK},
        {gsr, GSR},
        {o, O, i, FF},
        {x, X, i, #{clk => gck, s => gsr}}
    ]}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gsr ~s ~s ~s~n", [Device, MC, Name]),
    {Name, experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    })}.

%%--------------------------------------------------------------------


fuses(MC, {matrix,
           [GR, GS],
           [{off, [off, off]},
            {s,   [off, on ]},
            {r,   [on , off]}
           ]
          }) ->
    [fuse(GR, MC, ff_r_gsr),
     fuse(GS, MC, ff_s_gsr)
    ].

%%--------------------------------------------------------------------

fuse(Fuse, MC, Extra) ->
    {Fuse, list_to_atom(io_lib:format("~s_~s", [MC, Extra]))}.

