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
    Inputs = read(Density),
    {common_input_choices, InputMap} = Inputs,
    {common_input_sources, SourceMap} = inputs:reverse(Inputs),
    Choices = density:input_choices(Density),
    MCs = density:macro_cells(Density),
    Device = density:largest_device(Density),
    IOs = device:io_macro_cells(Device),
    Source = source(Module, InputMap, SourceMap, Choices, MCs, IOs),
    ok = file:write_file(File, Source).

%%--------------------------------------------------------------------

read(Density) ->
    inputs:common(inputs:merge(inputs:read(Density), manual(Density))).

%%--------------------------------------------------------------------

manual(xc9572xl) ->
    [{fb01, input29, 19, {mc04_13, internal}}];
manual(_) ->
    [].

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
    "-type macro_cell() :: macro_cell:absolute().\n"
    "-type choice() :: input:choice().\n"
    "-type realm() :: input:realm().\n"
    "-type source() :: input:source().\n"
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
    "-spec choice(input(), choice()) -> source() | unknown.\n"
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

choice_clause(Input, Choice, {MC, Realm}) ->
    clause(io_lib:format("choice(~s, ~2b) -> {~s, ~s}", [
        Input,
        Choice,
        MC,
        Realm
    ]));
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
    "-spec sources(macro_cell(), realm())\n"
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
                sources_realm(MC, external, SourceMap, unknown),
                sources_realm(MC, internal, SourceMap, unknown)
                |
                Clauses
            ];

        false ->
            [
                sources_realm(MC, external, SourceMap, no_pin),
                sources_realm(MC, internal, SourceMap, unknown)
                |
                Clauses
            ]
    end.

%%--------------------------------------------------------------------

sources_realm(MC, Realm, SourceMap, Default) ->
    case maps:get({MC, Realm}, SourceMap, #{}) of
        InputMap when map_size(InputMap) =:= 0 ->
            sources_clause(MC, Realm, Default);

        InputMap ->
            Inputs = lists:sort(maps:keys(InputMap)),
            Choice = sources_choice(maps:values(InputMap)),
            sources_clause(MC, Realm, {Choice, Inputs})
    end.

%%--------------------------------------------------------------------

sources_choice([C]) -> C;
sources_choice([C, C]) -> C;
sources_choice([C, C, C]) -> C;
sources_choice([C, C, C, C]) -> C.

%%--------------------------------------------------------------------

sources_clause(MC, Realm, {Choice, [A]}) ->
    clause(io_lib:format("sources(~s, ~s) -> {~2b, [~s]}", [
        MC,
        Realm,
        Choice,
        A
    ]));
sources_clause(MC, Realm, {Choice, [A, B]}) ->
    clause(io_lib:format("sources(~s, ~s) -> {~2b, [~s, ~s]}", [
        MC,
        Realm,
        Choice,
        A, B
    ]));
sources_clause(MC, Realm, {Choice, [A, B, C]}) ->
    clause(io_lib:format("sources(~s, ~s) -> {~2b, [~s, ~s, ~s]}", [
        MC,
        Realm,
        Choice,
        A, B, C
    ]));
sources_clause(MC, Realm, {Choice, [A, B, C, D]}) ->
    clause(io_lib:format("sources(~s, ~s) -> {~2b, [~s, ~s, ~s, ~s]}", [
        MC,
        Realm,
        Choice,
        A, B, C, D
    ]));
sources_clause(MC, Realm, Sources) ->
    clause(io_lib:format("sources(~s, ~s) -> ~p", [MC, Realm, Sources])).

