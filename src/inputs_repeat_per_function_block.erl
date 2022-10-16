-module(inputs_repeat_per_function_block).

-export([run/0]).

%%  Confirm that:
%%  
%%      The mapping
%%       - from input (1..54) choice (1..c, 16..15+c)
%%       - to macro cell input/ooutput
%%      is the same for ALL function blocks

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).
    %density(xc95144xl).

%%--------------------------------------------------------------------

density(Density) ->
    io:format("inputs-repeat-per-funcion-block ~p~n", [Density]),
    Inputs = repeat_per_function_block(Density),
    case missing_choices(Density, Inputs) of
        [] ->
            ok;

        Missing ->
            io:format("MISSING ~p~n", [Missing])
    end,
    %{input_sources, #{fb01 := Reverse}} = inputs:reverse(
    %    {input_choices, #{fb01 => Inputs}}
    %),
    %io:format("~s~n", [io_lib:format("~p", [Reverse])]),
    ok.

%%--------------------------------------------------------------------

repeat_per_function_block(Density) ->
    {input_choices, FBs} = inputs:read(Density),
    FBCount = density:function_block_count(Density),
    FBCount = map_size(FBs),
    maps:fold(fun merge_fb/3, #{}, FBs).

%%--------------------------------------------------------------------

merge_fb(_FB, Inputs, MergedInputs) ->
    maps:fold(fun merge_input/3, Inputs, MergedInputs).

%%--------------------------------------------------------------------

merge_input(Input, Choices, MergedInputs) ->
    MergedChoices0 = maps:get(Input, MergedInputs, #{}),
    MergedChoices = maps:fold(fun merge_choice/3, Choices, MergedChoices0),
    MergedInputs#{Input => MergedChoices}.

%%--------------------------------------------------------------------

merge_choice(Choice, Source, MergedChoices) ->
    case MergedChoices of
        #{Choice := Existing} when Existing =:= Source ->
            MergedChoices;

        #{Choice := Existing} ->
            throw({Choice, existing, Existing, merge, Source});

        _ ->
            MergedChoices#{Choice => Source}
    end.

%%--------------------------------------------------------------------

missing_choices(Density, Inputs) ->
    Expect = density:input_choices(Density),
    Fold = fun (Input, Choices, Missing) ->
        missing_choices(Input, Choices, Missing, Expect)
    end,
    lists:sort(maps:fold(Fold, [], Inputs)).

%%--------------------------------------------------------------------

missing_choices(Input, ChoicesMap, Missing, Expect) ->
    Choices = lists:sort(maps:keys(ChoicesMap)),
    % we do not expect any additional choices
    [] = lists:subtract(Choices, Expect),
    %
    Fold = fun(Miss, Missing0) ->
        [{Miss, Input} | Missing0]
    end,
    lists:foldl(Fold, Missing, lists:subtract(Expect, Choices)).


