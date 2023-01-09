-module(inputs).

-export([common/1]).
-export([merge/2]).
-export([read/1]).
-export([reverse_and_write_all/0]).
-export([reverse/1]).
-export([update/2]).
-export([write/2]).
-export([names/2]).

-export_type([common_input_choices/0]).
-export_type([common_input_sources/0]).
-export_type([raw_input_choices/0]).
-export_type([raw_input_sources/0]).
-export_type([merge/0]).

-type choice() :: input:choice().
-type density() :: density:density().
-type device() :: device:device().
-type function_block() :: function_block:function_block().
-type input() :: input:input().
-type realm() :: input:realm().
-type source() :: input:source().

-type merge() :: {function_block(), input(), choice(), source()}.

-type raw_input_choices() :: {input_choices, function_block_choices()}.
-type common_input_choices() :: {common_input_choices, input_choices()}.

-type function_block_choices() :: #{ function_block() => input_choices() }.
-type input_choices() :: #{ input() => choices() }.
-type choices() :: #{ choice() => source() }.

-type raw_input_sources() :: {input_sources, function_block_sources()}.
-type common_input_sources() :: {common_input_sources, source_sources()}.

-type function_block_sources() :: #{ function_block() => source_sources() }.
-type source_sources() :: #{ source() => input_sources() }.
-type input_sources() :: #{ input() => choice() }.

%%====================================================================
%% common
%%====================================================================

-spec common(raw_input_choices()) -> common_input_choices().

common({input_choices, FBs}) ->
    Init = maps:from_list([
        {Input, #{}}
        ||
        Input <- input:list()
    ]),
    Inputs = maps:fold(fun common_function_block/3, Init, FBs),
    {common_input_choices, Inputs}.

%%--------------------------------------------------------------------

common_function_block(_FB, Inputs, CommonInputs) ->
    maps:fold(fun common_input/3, Inputs, CommonInputs).

%%--------------------------------------------------------------------

common_input(Input, Choices, CommonInputs) ->
    CommonChoices0 = maps:get(Input, CommonInputs, #{}),
    CommonChoices = maps:fold(fun common_choice/3, Choices, CommonChoices0),
    CommonInputs#{Input => CommonChoices}.

%%--------------------------------------------------------------------

common_choice(Choice, Source, CommonChoices) ->
    case CommonChoices of
        #{Choice := Existing} when Existing =:= Source ->
            CommonChoices;

        #{Choice := Existing} ->
            throw({Choice, existing, Existing, merge, Source});

        _ ->
            CommonChoices#{Choice => Source}
    end.

%%====================================================================
%% merge
%%====================================================================

-spec merge(raw_input_choices(), [merge()]) -> raw_input_choices().

merge(Inputs = {input_choices, _}, []) ->
    Inputs;
merge({input_choices, FBs}, Items) when is_list(Items) ->
    {input_choices, merge_list(FBs, Items)}.

%%--------------------------------------------------------------------

merge_list(FBs, []) ->
    FBs;
merge_list(FBs, [Item | Items]) ->
    merge_list(merge_item(FBs, Item), Items).

%%--------------------------------------------------------------------

merge_item(FBs, {FB, Input, Choice, Source}) ->
    case FBs of
        #{FB := #{Input := #{Choice := Source}}} ->
            FBs;

        #{FB := #{Input := #{Choice := Already}}} ->
            throw({input, FB, Input, Choice, merge, Source, already, Already});

        #{FB := Inputs = #{Input := Choices}} ->
            FBs#{FB => Inputs#{Input => Choices#{Choice => Source}}};

        #{FB := Inputs} ->
            FBs#{FB => Inputs#{Input => #{Choice => Source}}};

        _ ->
            FBs#{FB => #{Input => #{Choice => Source}}}
    end.

%%====================================================================
%% read
%%====================================================================

-spec read(density() | device()) -> raw_input_choices().

read(DensityOrDevice) ->
    Density = density:or_device(DensityOrDevice),
    File = choices_file(Density),
    {ok, Binary} = file:read_file(File),
    Lines = binary:split(Binary, <<"\n">>, [global]),
    {input_choices, lists:foldl(fun read_line/2, #{}, Lines)}.

%%--------------------------------------------------------------------

read_line(<<>>, FBs) ->
    FBs;
read_line(Line, FBs) ->
    % fb01,input01: mc02_09i mc01_01i
    <<"fb", FB_:2/binary, ",input", Input_:2/binary, ":", Rest/binary>> = Line,
    FB = function_block:from(binary_to_integer(FB_)),
    Input = input:from(binary_to_integer(Input_)),
    case FBs of
        #{FB := Inputs = #{Input := Choices0}} ->
            Choices = read_choices(1, Rest, Choices0),
            FBs#{FB => Inputs#{Input => Choices}};

        #{FB := Inputs} ->
            Choices = read_choices(1, Rest, #{}),
            FBs#{FB => Inputs#{Input => Choices}};

        _ ->
            Choices = read_choices(1, Rest, #{}),
            FBs#{FB => #{Input => Choices}}
    end.

%%--------------------------------------------------------------------

read_choices(_, <<>>, Choices) ->
    Choices;
read_choices(Choice, <<"         ", Rest/binary>>, Choices) ->
    read_choices(Choice + 1, Rest, Choices);
read_choices(Choice, Line, Choices) ->
    <<" mc", FB_:2/binary, "_", MC_:2/binary, Realm_, Rest/binary>> = Line,
    MC = macro_cell:from(binary_to_integer(FB_), binary_to_integer(MC_)), 
    Realm = read_realm(Realm_),
    read_choices(Choice + 1, Rest, Choices#{Choice => {MC, Realm}}).

%%--------------------------------------------------------------------

-spec read_realm(char()) -> realm().

read_realm($e) -> external;
read_realm($i) -> internal.

%%====================================================================
%% reverse
%%====================================================================

-spec reverse_and_write_all() -> ok.

reverse_and_write_all() ->
    lists:foreach(fun (Density) ->
        Choices = read(Density),
        Sources = reverse(Choices),
        write(Density, Sources)
    end, density:list()).

%%--------------------------------------------------------------------

-spec reverse
    (raw_input_choices()) -> raw_input_sources();
    (common_input_choices()) -> common_input_sources().

reverse({input_choices, FBs}) ->
    {input_sources, maps:map(fun reverse_fb/2, FBs)};
reverse({common_input_choices, Inputs}) ->
    {common_input_sources, reverse_fb(undefined, Inputs)}.

%%--------------------------------------------------------------------

reverse_fb(_FB, Inputs) ->
    maps:fold(fun reverse_input/3, #{}, Inputs).

%%--------------------------------------------------------------------

reverse_input(Input, Choices, Sources0) ->
    Fold = fun (Choice, Source, Sources) ->
        reverse_slot(Input, Choice, Source, Sources)
    end,
    maps:fold(Fold, Sources0, Choices).

%%--------------------------------------------------------------------

reverse_slot(Input, Choice, Source, Sources) ->
    case Sources of
        #{Source := Inputs} ->
            Sources#{Source => Inputs#{Input => Choice}};

        _ ->
            Sources#{Source => #{Input => Choice}}
    end.

%%====================================================================
%% update
%%====================================================================

-spec update(density() | device(), [merge()]) -> ok.

update(DensityOrDevice, Items) ->
    write(DensityOrDevice, merge(read(DensityOrDevice), Items)).

%%====================================================================
%% write
%%====================================================================

-spec write(density() | device(), raw_input_choices() | raw_input_sources())
    -> ok.

write(DensityOrDevice, {input_choices, FBs}) ->
    Density = density:or_device(DensityOrDevice),
    File = choices_file(Density),
    Lines = [
        write_choices_fb(FB, FBs)
        ||
        FB <- density:function_blocks(Density)
    ],
    ok = file:write_file(File, Lines);
write(DensityOrDevice, {input_sources, FBs}) ->
    Density = density:or_device(DensityOrDevice),
    File = sources_file(Density),
    MCs = density:macro_cells(Density),
    Lines = [
        write_sources_fb(FB, FBs, MCs)
        ||
        FB <- density:function_blocks(Density)
    ],
    ok = file:write_file(File, Lines).

%%--------------------------------------------------------------------

write_choices_fb(FB, FBs) ->
    Inputs = maps:get(FB, FBs, #{}),
    [
        write_choices_line(FB, Input, Inputs)
        ||
        Input <- input:list()
    ].

%%--------------------------------------------------------------------

write_choices_line(FB, Input, Inputs) ->
    Choices = lists:sort(maps:to_list(maps:get(Input, Inputs, #{}))),
    % fb01,input01: mc02_09i mc01_01i
    [
        io_lib:format("~p,~p:", [FB, Input])
        |
        write_choices_entry(1, Choices, [])
    ].

%%--------------------------------------------------------------------

write_choices_entry(_, [], Line) ->
    lists:reverse(Line, [<<"\n">>]);
write_choices_entry(Next, Slots = [{Slot, _} | _], Line) when Next < Slot ->
    write_choices_entry(Next + 1, Slots, [<<"         ">> | Line]);
write_choices_entry(Slot, [{Slot, {MC, Realm}} | Slots], Line) ->
    Head = io_lib:format(" ~p~p", [MC, write_realm(Realm)]),
    write_choices_entry(Slot + 1, Slots, [Head | Line]).

%%--------------------------------------------------------------------

write_sources_fb(FB, FBs, MCs) ->
    Sources = maps:get(FB, FBs, #{}),
    [[
        write_sources_line(FB, MC, external, Sources)
        ||
        MC <- MCs
     ],
     [
        write_sources_line(FB, MC, internal, Sources)
        ||
        MC <- MCs
    ]].

%%--------------------------------------------------------------------

write_sources_line(FB, MC, Realm, Sources) ->
    Inputs = maps:get({MC, Realm}, Sources, #{}),
    % fb01,mc01_01i: input01,3 input27,3
    [
        io_lib:format("~p,~p~p:", [FB, MC, write_realm(Realm)]),
        [
            io_lib:format(" ~p,~p", [Input, Choice])
            ||
            {Input, Choice} <- lists:sort(maps:to_list(Inputs))
        ],
        <<"\n">>
    ].

%%--------------------------------------------------------------------

write_realm(external) -> e;
write_realm(internal) -> i.

%%====================================================================
%% names
%%====================================================================

names(Inputs, {common_input_choices, Choices}) ->
    maps:map(fun (_, Is) ->
        maps:map(fun (I, N) ->
            #{I := Ns} = Choices,
            case Ns of
                #{N := S} ->
                    S;

                _ ->
                    N
            end
        end, Is)
    end, Inputs).

%%====================================================================
%% helpers
%%====================================================================

choices_file(Density) ->
    lists:flatten(io_lib:format("database/~s-input-choices.txt", [Density])).

%%--------------------------------------------------------------------

sources_file(Density) ->
    lists:flatten(io_lib:format("database/~s-input-sources.txt", [Density])).

