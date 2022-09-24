-module(gsr_invert_experiment).

-export([run/0]).

%%  Detect the global GSR invert fuse.
%%
%%  When used the GSR can be inverted at the global level.

%%====================================================================
%% run
%%====================================================================

run() ->
    [ run(Density) || Density <- density:list() ],
    ok.

%%--------------------------------------------------------------------

run(Density) ->
    Device = density:largest_device(Density),
    FBs = density:function_blocks(Density),
    IOs = device:io_macro_cells(Device),
    GSR = device:gsr_macro_cell(Device),
    [GCK | _] = device:gck_macro_cells(Device),
    I = input_macro_cell(IOs, GSR, GCK),
    Os = [ output_macro_cell(FB, IOs, GSR, GCK, I) || FB <- FBs ],
    High = experiment(Device, source(high, GSR, GCK, I, Os)),
    Low = experiment(Device, source(low, GSR, GCK, I, Os)),
    {[Fuse], []} = fuses:diff(High, Low),
    Fuses = [{Fuse, gsr_invert}],
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

input_macro_cell([GSR | IOs], GSR, GCK) ->
    input_macro_cell(IOs, GSR, GCK);
input_macro_cell([GCK | IOs], GSR, GCK) ->
    input_macro_cell(IOs, GSR, GCK);
input_macro_cell([IO | _], _, _) ->
    IO.

%%--------------------------------------------------------------------

output_macro_cell(FB, [GSR | IOs], GSR, GCK, I) ->
    output_macro_cell(FB, IOs, GSR, GCK, I);
output_macro_cell(FB, [GCK | IOs], GSR, GCK, I) ->
    output_macro_cell(FB, IOs, GSR, GCK, I);
output_macro_cell(FB, [I | IOs], GSR, GCK, I) ->
    output_macro_cell(FB, IOs, GSR, GCK, I);
output_macro_cell(FB, [IO | IOs], GSR, GCK, I) ->
    case macro_cell:function_block(IO) of
        FB ->
            {FB, IO};

        _ ->
            output_macro_cell(FB, IOs, GSR, GCK, I)
    end.

%%--------------------------------------------------------------------

source(Name, GSR, GCK, I, Os) ->
    Reset = case Name of
        high -> gsr;
        low -> {low, gsr}
    end,
    {Name, [
        {i, I},
        {gck, GCK},
        {gsr, GSR}
        |
        [ signal(O, Reset) || O <- Os ]
    ]}.

%%--------------------------------------------------------------------

signal({FB, MC}, Reset) ->
    {FB, MC, i, #{clk => gck, r => Reset}}.

%%--------------------------------------------------------------------

experiment(Device, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gsr-invert ~s ~s~n", [Device, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    experiment:cached_jed(Cache).

