-module(generate_input_map).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    Module = io_lib:format("~s_input_map", [Density]),
    Name = lists:flatten(io_lib:format("~s.erl", [Module])),
    File = filename:join("src", Name),
    Inputs = inputs:common(inputs:read(Density)),
    {common_input_choices, InputMap} = Inputs,
    {common_input_sources, SourceMap} = inputs:reverse(Inputs),
    Choices = density:input_choices(Density),
    MCs = density:macro_cells(Density),
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    Source = source(Module, InputMap, SourceMap, Choices, MCs, IOs),
    ok = file:write_file(File, Source).

%%====================================================================
%% source
%%====================================================================

source(Module, InputMap, SourceMap, Choices, MCs, IOs) ->
    [<<
    "-module(">>, Module, <<").\n"
    "\n"
    "-export([choices/0]).\n"
    "-export([choice/2]).\n"
    "-export([sources/2]).\n"
    "\n"
    "-type input() :: input:input().\n"
    "-type macro_cell() :: macro_cell:macro_cell().\n"
    "\n"
    "-type choice() :: non_neg_integer().\n"
    "-type direction() :: input | output.\n"
    "\n">>,
    choices(Choices),
    choice(InputMap, Choices),
    sources(SourceMap, MCs, IOs)
    ].

%%--------------------------------------------------------------------

section(Name) ->
    <<
    "%%====================================================================\n"
    "%% ", Name/binary, "\n"
    "%%====================================================================\n"
    >>.

%%--------------------------------------------------------------------

clause(Clause) ->
    [Clause, <<";\n">>].

%%--------------------------------------------------------------------

clauses([]) ->
    [];
clauses([[Clause, <<";\n">>] | Clauses]) ->
    lists:reverse(Clauses, [Clause, <<".\n">>]).

%%====================================================================
%% choices
%%====================================================================

choices(Choices) ->
    List = io_lib:format("~p", [Choices]),
    [
    section(<<"choices">>), <<
    "\n"
    "-spec choices() -> [choice()].\n"
    "\n"
    "choices() ->\n"
    "    ">>, List, <<".\n"
    "\n">>
    ].

%%====================================================================
%% choice
%%====================================================================

choice(InputMap, Choices) ->
    Fold = fun (Input, Acc) ->
        choice_input(Input, InputMap, Choices, Acc)
    end,
    Clauses = clauses(lists:foldl(Fold, [], input:list())),
    [
    section(<<"choice">>), <<
    "\n"
    "-spec choice(input(), choice())\n"
    "    -> {macro_cell(), direction()} | unknown.\n"
    "\n">>,
    Clauses, <<
    "\n">>
    ].

%%--------------------------------------------------------------------

choice_input(Input, InputMap, Choices, Clauses) ->
    ChoiceMap = maps:get(Input, InputMap, #{}),
    Fold = fun (Choice, Acc) ->
        choice_choice(Input, Choice, ChoiceMap, Acc)
    end,
    lists:foldl(Fold, Clauses, Choices).

%%--------------------------------------------------------------------

choice_choice(Input, Choice, ChoiceMap, Clauses) ->
    Source = maps:get(Choice, ChoiceMap, unknown),
    Clause = choice_clause(Input, Choice, Source),
    [Clause | Clauses].

%%--------------------------------------------------------------------

choice_clause(Input, Choice, Source) ->
    clause(io_lib:format("choice(~s, ~2b) -> ~p", [Input, Choice, Source])).

%%====================================================================
%% sources
%%====================================================================

sources(SourceMap, MCs, IOs) ->
    Fold = fun (MC, Acc) ->
        sources_mc(MC, SourceMap, IOs, Acc)
    end,
    Clauses = clauses(lists:foldl(Fold, [], MCs)),
    [
    section(<<"sources">>), <<
    "\n"
    "-spec sources(macro_cell(), direction())\n"
    "    -> {choice(), [input()]} | no_pin | unknown.\n"
    "\n">>,
    Clauses, <<
    "\n">>
    ].

%%--------------------------------------------------------------------

sources_mc(MC, SourceMap, IOs, Clauses) ->
    case lists:member(MC, IOs) of
        true ->
            [
                sources_dir(MC, input, SourceMap),
                sources_dir(MC, output, SourceMap)
                |
                Clauses
            ];

        false ->
            [
                sources_dir(MC, input, SourceMap),
                sources_clause(MC, output, no_pin)
                |
                Clauses
            ]
    end.

%%--------------------------------------------------------------------

sources_dir(MC, Dir, SourceMap) ->
    case maps:get({MC, Dir}, SourceMap, #{}) of
        InputMap when map_size(InputMap) =:= 0 ->
            sources_clause(MC, Dir, unknown);

        InputMap ->
            Inputs = lists:sort(maps:keys(InputMap)),
            Choice = sources_choice(maps:values(InputMap)),
            sources_clause(MC, Dir, {Choice, Inputs})
    end.

%%--------------------------------------------------------------------

sources_choice([C]) -> C;
sources_choice([C, C]) -> C;
sources_choice([C, C, C]) -> C;
sources_choice([C, C, C, C]) -> C.

%%--------------------------------------------------------------------

sources_clause(MC, Dir, Sources) ->
    clause(io_lib:format("sources(~s, ~s) -> ~p", [MC, Dir, Sources])).

