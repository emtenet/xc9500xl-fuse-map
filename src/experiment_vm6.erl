-module(experiment_vm6).

-export([imux/1]).

% Example experiment.vm6 file:
%
%   ...
%
%   FB_INSTANCE | FOOBAR1_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
%   FBPIN | 1 | z_OBUF$BUF1 | 1 | NULL | 0 | x | 1 | H3 | 49152
%   FBPIN | 2 | z_OBUF$BUF0 | 1 | NULL | 0 | y | 1 | F1 | 49152
%   FBPIN | 3 | z_OBUF | 1 | NULL | 0 | z | 1 | G2 | 49152
%
%   FB_INSTANCE | FOOBAR2_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
%   FBPIN | 5 | NULL | 0 | oe1_IBUF | 0 | NULL | 0 | B1 | 53248
%   FBPIN | 6 | NULL | 0 | oe2_IBUF | 0 | NULL | 0 | C2 | 53248
%   FBPIN | 10 | NULL | 0 | a_IBUF | 1 | NULL | 0 | D2 | 49152
%   FBPIN | 11 | NULL | 0 | b_IBUF | 1 | NULL | 0 | E4 | 49152
%   FBPIN | 12 | NULL | 0 | c_IBUF | 1 | NULL | 0 | E3 | 49152
%   FBPIN | 14 | NULL | 0 | d_IBUF | 1 | NULL | 0 | E2 | 49152
%
%   FB_INSTANCE | FOOBAR3_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
%
%   FB_INSTANCE | FOOBAR4_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
%
%   FB_INSTANCE | INPUTPINS_FOOBAR9_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
%
%
%   FB_ORDER_OF_INPUTS | FOOBAR1_ | 0 | a | D2 | 6 | d | E2 | 49 | b | E4 | 53 | c | E3
%
%   FB_IMUX_INDEX | FOOBAR1_ | 192 | -1 | -1 | -1 | -1 | -1 | 198 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | 194 | -1 | -1 | -1 | 196
%
%   FB_ORDER_OF_INPUTS | FOOBAR3_ | 26 | i | K2
%
%   FB_IMUX_INDEX | FOOBAR3_ | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | 384 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1 | -1
%
%   ...

%%====================================================================
%% imux
%%====================================================================

imux(File) ->
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\n">>, [global]),
    imux_instance(Lines, #{}).

%%--------------------------------------------------------------------

imux_instance([<<"FB_INSTANCE | INPUTPINS_", _/binary>> | Lines], Pins) ->
    imux_inputs(Lines, #{}, Pins);
imux_instance([Line = <<"FB_INSTANCE ", _/binary>> | Lines], Pins) ->
    % FB_INSTANCE | FOOBAR1_ | experiment_COPY_0_COPY_0 | 0 | 0 | 0
    [_, FB_ | _] = binary:split(Line, <<" | ">>, [global]),
    FBNumber = imux_fb_number(FB_),
    imux_pins(Lines, FBNumber, Pins);
imux_instance([_ | Lines], Pins) ->
    imux_instance(Lines, Pins).

%%--------------------------------------------------------------------

imux_fb(<<"FOOBAR1_">>) -> fb01;
imux_fb(<<"FOOBAR2_">>) -> fb02;
imux_fb(<<"FOOBAR3_">>) -> fb03;
imux_fb(<<"FOOBAR4_">>) -> fb04;
imux_fb(<<"FOOBAR5_">>) -> fb05;
imux_fb(<<"FOOBAR6_">>) -> fb06;
imux_fb(<<"FOOBAR7_">>) -> fb07;
imux_fb(<<"FOOBAR8_">>) -> fb08;
imux_fb(<<"FOOBAR9_">>) -> fb09;
imux_fb(<<"FOOBAR10_">>) -> fb10;
imux_fb(<<"FOOBAR11_">>) -> fb11;
imux_fb(<<"FOOBAR12_">>) -> fb12;
imux_fb(<<"FOOBAR13_">>) -> fb13;
imux_fb(<<"FOOBAR14_">>) -> fb14;
imux_fb(<<"FOOBAR15_">>) -> fb15;
imux_fb(<<"FOOBAR16_">>) -> fb16;
imux_fb(<<"FOOBAR17_">>) -> fb17;
imux_fb(<<"FOOBAR18_">>) -> fb18.

%%--------------------------------------------------------------------

imux_fb_number(<<"FOOBAR1_">>) -> 1;
imux_fb_number(<<"FOOBAR2_">>) -> 2;
imux_fb_number(<<"FOOBAR3_">>) -> 3;
imux_fb_number(<<"FOOBAR4_">>) -> 4;
imux_fb_number(<<"FOOBAR5_">>) -> 5;
imux_fb_number(<<"FOOBAR6_">>) -> 6;
imux_fb_number(<<"FOOBAR7_">>) -> 7;
imux_fb_number(<<"FOOBAR8_">>) -> 8;
imux_fb_number(<<"FOOBAR9_">>) -> 9;
imux_fb_number(<<"FOOBAR10_">>) -> 10;
imux_fb_number(<<"FOOBAR11_">>) -> 11;
imux_fb_number(<<"FOOBAR12_">>) -> 12;
imux_fb_number(<<"FOOBAR13_">>) -> 13;
imux_fb_number(<<"FOOBAR14_">>) -> 14;
imux_fb_number(<<"FOOBAR15_">>) -> 15;
imux_fb_number(<<"FOOBAR16_">>) -> 16;
imux_fb_number(<<"FOOBAR17_">>) -> 17;
imux_fb_number(<<"FOOBAR18_">>) -> 18.

%%--------------------------------------------------------------------

imux_mc(FBNumber, MC) ->
    MCNumber = binary_to_integer(MC),
    macro_cell:from(FBNumber, MCNumber).

%%--------------------------------------------------------------------

imux_pins([Line = <<"FBPIN ", _/binary>> | Lines], FBNumber, Pins) ->
    % FBPIN | 14 | NULL | 0 | d_IBUF | 1 | NULL | 0 | E2 | 49152
    % FBPIN | 15 | hidden | 1 | NULL | 0 | NULL | 0 | C2 | 49152
    [_, MC_, Name, _, _, _, _, _, Pin, _]
        = binary:split(Line, <<" | ">>, [global]),
    MC = imux_mc(FBNumber, MC_),
    case Name of
        <<"NULL">> ->
            imux_pins(Lines, FBNumber, Pins#{Pin => MC});

        _ ->
            imux_pins(Lines, FBNumber, Pins#{Pin => MC, Name => MC})
    end;
imux_pins([<<>> | Lines], _, Pins) ->
    imux_instance(Lines, Pins).

%%--------------------------------------------------------------------

imux_inputs([], FBs, _Pins) ->
    FBs;
imux_inputs([Line = <<"FB_ORDER_OF_INPUTS ", _/binary>> | Lines], FBs, Pins) ->
    % FB_ORDER_OF_INPUTS | FOOBAR1_ | 0 | a | D2 | 6 | d | E2 | 49 | b | E4
    [_, FB_ | Inputs] = binary:split(Line, <<" | ">>, [global]),
    FB = imux_fb(FB_),
    imux_inputs(Lines, imux_inputs(Inputs, FB, FBs, Pins), Pins);
imux_inputs([_ | Lines], FBs, Pins) ->
    imux_inputs(Lines, FBs, Pins).

%%--------------------------------------------------------------------

imux_inputs([], _, FBs, _) ->
    FBs;
imux_inputs([Index_, Name, <<"NULL">> | Inputs], FB, FBs, Pins) ->
    % convert index from 0-based to 1-based
    Input = input:from(1 + binary_to_integer(Index_)),
    #{Name := MC} = Pins,
    imux_inputs(Inputs, FB, imux_input(FB, output, MC, Input, FBs), Pins);
imux_inputs([Index_, _Name, Pin | Inputs], FB, FBs, Pins) ->
    % convert index from 0-based to 1-based
    Input = input:from(1 + binary_to_integer(Index_)),
    #{Pin := MC} = Pins,
    imux_inputs(Inputs, FB, imux_input(FB, input, MC, Input, FBs), Pins).

%%--------------------------------------------------------------------

imux_input(FB, Type, MC, Input, FBs) ->
    case FBs of
        #{FB := Types = #{Type := MCs}} ->
            FBs#{FB => Types#{Type => MCs#{MC => Input}}};

        #{FB := Types} ->
            FBs#{FB => Types#{Type => #{MC => Input}}};

        _ ->
            FBs#{FB => #{Type => #{MC => Input}}}
    end.

