-module(user_bits_experiment).

-export([run/0]).

-define(BASE, <<"@@@@">>).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun run/1, density:list()).

%%--------------------------------------------------------------------

run(Density) ->
    [Device | _] = density:devices(Density),
    io:format(" => user-bits ~s~n", [Density]),
    Base = {Device, ?BASE, experiment(Device, ?BASE)},
    U28 = add1(Base, <<"P@@@">>),
    U29 = add1(Base, <<"`@@@">>),
    U30 = del1_add2(Base, <<"0@@@">>, U28, U29),
    U31 = add1_del1(Base, <<128, "@@@">>, U30),
    U27 = add1(Base, <<"A@@@">>),
    U26 = add1(Base, <<"B@@@">>),
    U25 = add1(Base, <<"D@@@">>),
    U24 = add1(Base, <<"H@@@">>),
    %
    U20 = add1(Base, <<"@P@@">>),
    U21 = add1(Base, <<"@`@@">>),
    U22 = del1_add2(Base, <<"@0@@">>, U20, U21),
    U23 = add1_del1(Base, <<"@", 128, "@@">>, U22),
    U19 = add1(Base, <<"@A@@">>),
    U18 = add1(Base, <<"@B@@">>),
    U17 = add1(Base, <<"@D@@">>),
    U16 = add1(Base, <<"@H@@">>),
    %
    U12 = add1(Base, <<"@@P@">>),
    U13 = add1(Base, <<"@@`@">>),
    U14 = del1_add2(Base, <<"@@0@">>, U12, U13),
    U15 = add1_del1(Base, <<"@@", 128, "@">>, U14),
    U11 = add1(Base, <<"@@A@">>),
    U10 = add1(Base, <<"@@B@">>),
    U09 = add1(Base, <<"@@D@">>),
    U08 = add1(Base, <<"@@H@">>),
    %
    U04 = add1(Base, <<"@@@P">>),
    U05 = add1(Base, <<"@@@`">>),
    U06 = del1_add2(Base, <<"@@@0">>, U04, U05),
    U07 = add1_del1(Base, <<"@@@", 128>>, U06),
    U03 = add1(Base, <<"@@@A">>),
    U02 = add1(Base, <<"@@@B">>),
    U01 = add1(Base, <<"@@@D">>),
    U00 = add1(Base, <<"@@@H">>),
    %
    fuses:update(Device, [
        {user_31, U31},
        {user_30, U30},
        {user_29, U29},
        {user_28, U28},
        {user_27, U27},
        {user_26, U26},
        {user_25, U25},
        {user_24, U24},
        {user_23, U23},
        {user_22, U22},
        {user_21, U21},
        {user_20, U20},
        {user_19, U19},
        {user_18, U18},
        {user_17, U17},
        {user_16, U16},
        {user_15, U15},
        {user_14, U14},
        {user_13, U13},
        {user_12, U12},
        {user_11, U11},
        {user_10, U10},
        {user_09, U09},
        {user_08, U08},
        {user_07, U07},
        {user_06, U06},
        {user_05, U05},
        {user_04, U04},
        {user_03, U03},
        {user_02, U02},
        {user_01, U01},
        {user_00, U00}
    ]).

%%--------------------------------------------------------------------

run({Device, Base, BaseFuses}, Diff) ->
    DiffFuses = experiment(Device, Diff),
    {Add, Del} = fuses:diff(BaseFuses, DiffFuses),
    io:format("~s -> ~s | add ~p del ~p~n", [Base, Diff, Add, Del]),
    {Add, Del}.

%%====================================================================
%% experiment
%%====================================================================

experiment(Device, UserCode) ->
    experiment:run(#{
        device => Device,
        ucf => <<>>,
        usercode => UserCode,
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "\n"
            "entity experiment is\n"
            "  Port ( output : out STD_LOGIC );\n"
            "end experiment;\n"
            "\n"
            "architecture Behavioral of experiment is begin\n"
            "  output <= '1';\n"
            "end Behavioral;\n"
        >>
    }),
    experiment:jed().

%%====================================================================
%% helpers
%%====================================================================

add1(Base, Diff) ->
    {[Add], []} = run(Base, Diff),
    Add.

%%--------------------------------------------------------------------

add1_del1(Base, Diff, Del) ->
    {[Add], [Del]} = run(Base, Diff),
    Add.

%%--------------------------------------------------------------------

del1_add2(Base, Diff, A, B) ->
    case run(Base, Diff) of
        {[A, B], [Del]} ->
            Del;

        {[B, A], [Del]} ->
            Del
    end.

