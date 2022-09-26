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
    With = experimient_for(Device),
    Base = {With, ?BASE, experiment(With, ?BASE)},
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
        {U31, user31},
        {U30, user30},
        {U29, user29},
        {U28, user28},
        {U27, user27},
        {U26, user26},
        {U25, user25},
        {U24, user24},
        {U23, user23},
        {U22, user22},
        {U21, user21},
        {U20, user20},
        {U19, user19},
        {U18, user18},
        {U17, user17},
        {U16, user16},
        {U15, user15},
        {U14, user14},
        {U13, user13},
        {U12, user12},
        {U11, user11},
        {U10, user10},
        {U09, user09},
        {U08, user08},
        {U07, user07},
        {U06, user06},
        {U05, user05},
        {U04, user04},
        {U03, user03},
        {U02, user02},
        {U01, user01},
        {U00, user00}
    ]).

%%--------------------------------------------------------------------

run({With, Base, BaseFuses}, Diff) ->
    DiffFuses = experiment(With, Diff),
    {Add, Del} = fuses:diff(BaseFuses, DiffFuses),
    io:format("~s -> ~s | add ~p del ~p~n", [Base, Diff, Add, Del]),
    {Add, Del}.

%%====================================================================
%% experiment
%%====================================================================

experimient_for(Device) ->
    #{
        device => Device,
        ucf => <<>>,
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
    }.

%%--------------------------------------------------------------------

experiment(With, UserCode) ->
    Cache = experiment:cache(With#{
        usercode => UserCode
    }),
    experiment:cached_jed(Cache).

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

