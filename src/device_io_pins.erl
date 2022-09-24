-module(device_io_pins).

-export([generate/0]).

-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_UPPER(C), (C >= $A andalso C =< $Z)).

%%====================================================================
%% generate
%%====================================================================

% Generate the device:io_pins/1 function from BSDL files.
%
% Include the pin name and which function block it is connected to.
%
% BSDL files count function blocks 0-based, convert to 1-based to
% match the data sheets and synthesis tools (UCF file).

generate() ->
    Devices = lists:map(fun device/1, device:list()),
    output(Devices).

%%--------------------------------------------------------------------

device(Device) ->
    File = filename:join("bsdl", atom_to_list(Device) ++ ".bsd"),
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    {Device, device(Lines, [])}.

%%--------------------------------------------------------------------

device([], Pins) ->
    lists:sort(Pins);
device([Line | Lines], Pins) ->
    case Line of
        <<"\t\"PB", FB:2/binary, "_", MC:2/binary, ":", Rest/binary>> ->
            Cell = macro_cell(FB, MC),
            device(Lines, [pin(Rest, Cell) | Pins]);

        _ ->
            device(Lines, Pins)
    end.

%%--------------------------------------------------------------------

macro_cell(FB_, MC_) ->
    FB = binary_to_integer(FB_),
    MC = binary_to_integer(MC_),
    % BSDL files are 0-based, convert to 1-base
    list_to_atom(io_lib:format("mc~2..0b_~2..0b", [FB + 1, MC + 1])).

%%--------------------------------------------------------------------

pin(<<U, Rest/binary>>, Cell) when ?IS_UPPER(U) ->
    L = U - $A + $a,
    pin_number(L, Rest, Cell);
pin(Rest, Cell) ->
    pin_number($p, Rest, Cell).

%%--------------------------------------------------------------------

pin_number(L, <<A, B, C, _/binary>>, Cell)
        when ?IS_DIGIT(A) andalso
             ?IS_DIGIT(B) andalso
             ?IS_DIGIT(C) ->
    {binary_to_atom(<<L, A, B, C>>, latin1),
     binary_to_atom(<<L, A, B, C>>, latin1),
     Cell};
pin_number(L, <<A, B, _/binary>>, Cell)
        when ?IS_DIGIT(A) andalso
             ?IS_DIGIT(B) ->
    {binary_to_atom(<<L, $0, A, B>>, latin1),
     binary_to_atom(<<L, A, B>>, latin1),
     Cell};
pin_number(L, <<A, _/binary>>, Cell)
        when ?IS_DIGIT(A) ->
    {binary_to_atom(<<L, $0, $0, A>>, latin1),
     binary_to_atom(<<L, A>>, latin1),
     Cell}.

%%--------------------------------------------------------------------

output([Device]) ->
    output_device(Device, $.);
output([Device | Devices]) ->
    output_device(Device, $;),
    output(Devices).

%%--------------------------------------------------------------------

output_device({Device, Pins}, End) ->
    io:format("io_pins(~s) ->~n    [~n", [Device]),
    output_pins(Pins),
    io:format("    ]~c~n", [End]).

%%--------------------------------------------------------------------

output_pins([Pin]) ->
    output_pin(Pin, "");
output_pins([Pin | Pins]) ->
    output_pin(Pin, ","),
    output_pins(Pins).

%%--------------------------------------------------------------------

output_pin({_Sorting, Pin, Cell}, End) ->
    io:format("        {~s, ~s}~s~n", [Pin, Cell, End]).

