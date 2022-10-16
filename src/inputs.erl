-module(inputs).

-export([merge/2]).
-export([read/1]).
-export([reverse_and_write_all/0]).
-export([reverse/1]).
-export([update/2]).
-export([write/2]).

%%====================================================================
%% merge
%%====================================================================

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
    <<" mc", FB_:2/binary, "_", MC_:2/binary, Dir_, Rest/binary>> = Line,
    MC = macro_cell:from(binary_to_integer(FB_), binary_to_integer(MC_)), 
    Dir = read_dir(Dir_),
    read_choices(Choice + 1, Rest, Choices#{Choice => {MC, Dir}}).

%%--------------------------------------------------------------------

read_dir($i) -> input;
read_dir($o) -> output.

%%====================================================================
%% reverse
%%====================================================================

reverse_and_write_all() ->
    lists:foreach(fun (Density) ->
        write(Density, reverse(read(Density)))
    end, density:list()).

%%--------------------------------------------------------------------

reverse({input_choices, FBs}) ->
    {input_sources, maps:map(fun reverse_fb/2, FBs)}.

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

update(DensityOrDevice, Items) ->
    write(DensityOrDevice, merge(read(DensityOrDevice), Items)).

%%====================================================================
%% write
%%====================================================================

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
    lists:reverse(Line, <<"\n">>);
write_choices_entry(Next, Slots = [{Slot, _} | _], Line) when Next < Slot ->
    write_choices_entry(Next + 1, Slots, [<<"         ">> | Line]);
write_choices_entry(Slot, [{Slot, {MC, Dir}} | Slots], Line) ->
    Head = io_lib:format(" ~p~p", [MC, write_dir(Dir)]),
    write_choices_entry(Slot + 1, Slots, [Head | Line]).

%%--------------------------------------------------------------------

write_sources_fb(FB, FBs, MCs) ->
    Sources = maps:get(FB, FBs, #{}),
    [[
        write_sources_line(FB, MC, input, Sources)
        ||
        MC <- MCs
     ],
     [
        write_sources_line(FB, MC, output, Sources)
        ||
        MC <- MCs
    ]].

%%--------------------------------------------------------------------

write_sources_line(FB, MC, Dir, Sources) ->
    Inputs = maps:get({MC, Dir}, Sources, #{}),
    % fb01,mc01_01i: input01,3 input27,3
    [
        io_lib:format("~p,~p~p:", [FB, MC, write_dir(Dir)]),
        [
            io_lib:format(" ~p,~p", [Input, Choice])
            ||
            {Input, Choice} <- lists:sort(maps:to_list(Inputs))
        ],
        <<"\n">>
    ].

%%--------------------------------------------------------------------

write_dir(input) -> i;
write_dir(output) -> o.

%%====================================================================
%% helpers
%%====================================================================

choices_file(Density) ->
    lists:flatten(io_lib:format("database/~s-input-choices.txt", [Density])).

%%--------------------------------------------------------------------

sources_file(Density) ->
    lists:flatten(io_lib:format("database/~s-input-sources.txt", [Density])).

