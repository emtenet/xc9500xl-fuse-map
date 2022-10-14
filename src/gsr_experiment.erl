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
    fuses:update(Density, Fuses).

%%--------------------------------------------------------------------

gck(MC, [MC, GCK | _]) ->
    GCK;
gck(_, [GCK | _]) ->
    GCK.

%%--------------------------------------------------------------------

mc(Device, O, Avail, GSR, GCK) ->
    Used = [O, GSR, GCK],
    {I, _} = unused(Avail, Used),
    Sources = [
        source(off, I, GSR, GCK, O),
        source(s, I, GSR, GCK, O),
        source(r, I, GSR, GCK, O)
    ],
    Answers = [ experiment(Device, O, Source) || Source <- Sources ],
    Matrix = matrix:diff(Answers),
    matrix:print(Matrix),
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

source(Name, I, GSR, GCK, O) ->
    FF = case Name of
        off -> #{clk => gck};
        s -> #{clk => gck, s => gsr};
        r -> #{clk => gck, r => gsr}
    end,
    {Name, [
        {i, I},
        {gck, GCK, #{global => gck}},
        {gsr, GSR, #{global => gsr}},
        {o, O, i, FF}
    ]}.

%%--------------------------------------------------------------------

experiment(Device, MC, {Name, Signals}) ->
    {UCF, VHDL} = experiment:compile(Signals),
    io:format(" => gsr ~s ~s ~s~n", [Device, MC, Name]),
    Cache = experiment:cache(#{
        device => Device,
        usercode => <<"@@@@">>,
        ucf => UCF,
        vhdl => VHDL
    }),
    {Name, experiment:cached_jed(Cache)}.

%%--------------------------------------------------------------------

% We can get redundant fuses in the matrix, for example:
%
%   2 3 3 5 5 5 5 6
%   0 4 5 2 2 2 2 6
%   2 6 5 7 7 8 8 8
%   1 5 1 3 3 6 6 7
%   6 4 8 4 5 2 3 2
%   *| | |*| |*| | | off
%    | |*| |*| |*|*| s
%    |*| | |*| |*|*| r
%
% We can ignore the on-off-off and off-on-on columns.
% Look for the off-off-on column for the RESET fuse (2nd in example)
% Look for the off-on-off column for the SET fuse (3rd in example)
%
% NOTE: This did not happen before disabling global pin optimizations
% and so probably happens when we probe an input or output pin that
% has a global feature available.

fuses(MC, {matrix, Fuses, [{off, Os}, {s, Ss}, {r, Rs}]}) ->
    case fuses(Fuses, Os, Ss, Rs, undefined, undefined) of
        {S, R} when S =/= undefined andalso R =/= undefied ->
            [fuse(R, MC, r_mux),
             fuse(S, MC, s_mux)
            ]
    end.

%%--------------------------------------------------------------------

fuses([], [], [], [], S, R) ->
    {S, R};
fuses([_ | Fuses], [on  | Os], [off | Ss], [off | Rs], S, R) ->
    fuses(Fuses, Os, Ss, Rs, S, R);
fuses([_ | Fuses], [off | Os], [on  | Ss], [on  | Rs], S, R) ->
    fuses(Fuses, Os, Ss, Rs, S, R);
fuses([F | Fuses], [off | Os], [off | Ss], [on  | Rs], S, undefined) ->
    fuses(Fuses, Os, Ss, Rs, S, F);
fuses([F | Fuses], [off | Os], [on  | Ss], [off | Rs], undefined, R) ->
    fuses(Fuses, Os, Ss, Rs, F, R).

%%--------------------------------------------------------------------

fuse(Fuse, MC, Name) ->
    {Fuse, fuse:macro_cell(MC, Name)}.

