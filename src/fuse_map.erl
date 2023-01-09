-module(fuse_map).

-export([fuse/2]).
-export([fuses/2]).
-export([inputs/2]).
-export([name/2]).

-export_type([fuse/0]).
-export_type([name/0]).
-export_type([global_feature/0]).
-export_type([function_block_feature/0]).
-export_type([macro_cell_feature/0]).

-type density() :: density:density().
-type fb() :: function_block:function_block().
-type input() :: input:input().
-type mc() :: macro_cell:macro_cell().
-type pt() :: product_term:product_term().
-type user() :: fuse:user().

-type fuse() :: non_neg_integer().
-type name() ::
    global_feature() |
    user() |
    {fb(), function_block_feature()} |
    {fb(), mc(), macro_cell_feature()} |
    {fb(), mc(), pt(), input()} |
    {fb(), mc(), pt(), input()} |
    undefined.

-type global_feature() ::
    gsr_invert |
    gck1_enable |
    gck2_enable |
    gck3_enable |
    gts1_enable |
    gts2_enable |
    gts3_enable |
    gts4_enable |
    keeper_disable.

-type function_block_feature() ::
    always |
    enable |
    forward.

-type macro_cell_feature() ::
    bypass |
    ce_or_r |
    ce_or_s |
    clk_invert |
    clk_mux0 |
    clk_mux1 |
    fast |
    ground |
    oe_invert |
    oe_gts |
    oe_gts_mux0 |
    oe_gts_mux1 |
    preset |
    pt1_mux0 |
    pt1_mux1 |
    pt1_std_power |
    pt2_mux0 |
    pt2_mux1 |
    pt2_std_power |
    pt3_mux0 |
    pt3_mux1 |
    pt3_std_power |
    pt4_mux0 |
    pt4_mux1 |
    pt4_std_power |
    pt5_mux0 |
    pt5_mux1 |
    pt5_std_power |
    r_gsr |
    s_gsr |
    std_power |
    t_type.

%%====================================================================
%% and_array
%%====================================================================

and_array_from_macro_cell(mc01) -> {0, 0};
and_array_from_macro_cell(mc04) -> {0, 1};
and_array_from_macro_cell(mc07) -> {0, 2};
and_array_from_macro_cell(mc10) -> {0, 3};
and_array_from_macro_cell(mc13) -> {0, 4};
and_array_from_macro_cell(mc16) -> {0, 5};
and_array_from_macro_cell(mc02) -> {1, 0};
and_array_from_macro_cell(mc05) -> {1, 1};
and_array_from_macro_cell(mc08) -> {1, 2};
and_array_from_macro_cell(mc11) -> {1, 3};
and_array_from_macro_cell(mc14) -> {1, 4};
and_array_from_macro_cell(mc17) -> {1, 5};
and_array_from_macro_cell(mc03) -> {2, 0};
and_array_from_macro_cell(mc06) -> {2, 1};
and_array_from_macro_cell(mc09) -> {2, 2};
and_array_from_macro_cell(mc12) -> {2, 3};
and_array_from_macro_cell(mc15) -> {2, 4};
and_array_from_macro_cell(mc18) -> {2, 5}.

%%--------------------------------------------------------------------

and_array_from_product_term(pt3) -> 0;
and_array_from_product_term(pt5) -> 1;
and_array_from_product_term(pt4) -> 2;
and_array_from_product_term(pt1) -> 3;
and_array_from_product_term(pt2) -> 4.

%%--------------------------------------------------------------------

and_array_to_macro_cell(0, 0) -> mc01;
and_array_to_macro_cell(0, 1) -> mc04;
and_array_to_macro_cell(0, 2) -> mc07;
and_array_to_macro_cell(0, 3) -> mc10;
and_array_to_macro_cell(0, 4) -> mc13;
and_array_to_macro_cell(0, 5) -> mc16;
and_array_to_macro_cell(1, 0) -> mc02;
and_array_to_macro_cell(1, 1) -> mc05;
and_array_to_macro_cell(1, 2) -> mc08;
and_array_to_macro_cell(1, 3) -> mc11;
and_array_to_macro_cell(1, 4) -> mc14;
and_array_to_macro_cell(1, 5) -> mc17;
and_array_to_macro_cell(2, 0) -> mc03;
and_array_to_macro_cell(2, 1) -> mc06;
and_array_to_macro_cell(2, 2) -> mc09;
and_array_to_macro_cell(2, 3) -> mc12;
and_array_to_macro_cell(2, 4) -> mc15;
and_array_to_macro_cell(2, 5) -> mc18.

%%--------------------------------------------------------------------

and_array_to_product_term(0) -> pt3;
and_array_to_product_term(1) -> pt5;
and_array_to_product_term(2) -> pt4;
and_array_to_product_term(3) -> pt1;
and_array_to_product_term(4) -> pt2.

%%====================================================================
%% feature
%%====================================================================

feature_from_macro_cell(mc01) -> {0, 6};
feature_from_macro_cell(mc02) -> {1, 6};
feature_from_macro_cell(mc03) -> {2, 6};
feature_from_macro_cell(mc04) -> {3, 6};
feature_from_macro_cell(mc05) -> {4, 6};
feature_from_macro_cell(mc06) -> {5, 6};
feature_from_macro_cell(mc07) -> {6, 6};
feature_from_macro_cell(mc08) -> {7, 6};
feature_from_macro_cell(mc09) -> {8, 6};
feature_from_macro_cell(mc10) -> {0, 7};
feature_from_macro_cell(mc11) -> {1, 7};
feature_from_macro_cell(mc12) -> {2, 7};
feature_from_macro_cell(mc13) -> {3, 7};
feature_from_macro_cell(mc14) -> {4, 7};
feature_from_macro_cell(mc15) -> {5, 7};
feature_from_macro_cell(mc16) -> {6, 7};
feature_from_macro_cell(mc17) -> {7, 7};
feature_from_macro_cell(mc18) -> {8, 7}.

%%--------------------------------------------------------------------

feature_to_macro_cell(0, 6) -> mc01;
feature_to_macro_cell(1, 6) -> mc02;
feature_to_macro_cell(2, 6) -> mc03;
feature_to_macro_cell(3, 6) -> mc04;
feature_to_macro_cell(4, 6) -> mc05;
feature_to_macro_cell(5, 6) -> mc06;
feature_to_macro_cell(6, 6) -> mc07;
feature_to_macro_cell(7, 6) -> mc08;
feature_to_macro_cell(8, 6) -> mc09;
feature_to_macro_cell(0, 7) -> mc10;
feature_to_macro_cell(1, 7) -> mc11;
feature_to_macro_cell(2, 7) -> mc12;
feature_to_macro_cell(3, 7) -> mc13;
feature_to_macro_cell(4, 7) -> mc14;
feature_to_macro_cell(5, 7) -> mc15;
feature_to_macro_cell(6, 7) -> mc16;
feature_to_macro_cell(7, 7) -> mc17;
feature_to_macro_cell(8, 7) -> mc18.

%%====================================================================
%% fuse
%%====================================================================

-spec fuse(density(), fuse()) -> name().

fuse(Density, Fuse) ->
    %io:format("fuse_map:fuse(~p, ~p)~n", [Density, Fuse]),
    FBCount = density:function_block_count(Density),
    %
    BandSize = FBCount * 108,
    Band = Fuse div BandSize,
    BandFuse = Fuse rem BandSize,
    %
    WideRowSize = FBCount * 8,
    WideRows = 9,
    WideRowsSize = WideRows * WideRowSize,
    %
    if
        BandFuse < WideRowsSize ->
            % in a wide row
            Row = BandFuse div WideRowSize,
            RowFuse = BandFuse rem WideRowSize,
            %
            FB = function_block:from((RowFuse div 8) + 1),
            Column = RowFuse rem 8,
            fuse(Band, FB, Row, Column);

        true -> 
            % in a slim row
            SlimRowSize = FBCount * 6,
            Row = ((BandFuse - WideRowsSize) div SlimRowSize) + WideRows,
            RowFuse = (BandFuse - WideRowsSize) rem SlimRowSize,
            %
            FB = function_block:from((RowFuse div 6) + 1),
            Column = RowFuse rem 6,
            fuse(Band, FB, Row, Column)
    end.

%%--------------------------------------------------------------------

fuse(Band, FB, Row, Column) when Column < 6 ->
    %io:format("  fuse(~p, ~p, ~p, ~p)~n", [Band, FB, Row, Column]),
    MC = and_array_to_macro_cell(Row div 5, Column),
    PT = and_array_to_product_term(Row rem 5),
    Input = input:from((Band div 2) + 1),
    case Band rem 2 of
        0 ->
            {FB, MC, PT, Input, invert};

        1 ->
            {FB, MC, PT, Input}
    end;
fuse(  0, FB, Row, Column) -> fuse_unknown(band001, FB, Row, Column);
fuse(  1, FB, Row, Column) -> fuse_unknown(band002, FB, Row, Column);
fuse(  2, FB, Row, Column) -> fuse_global(FB, Row, Column);
fuse(  3, FB, Row, Column) -> fuse_unknown(band004, FB, Row, Column);
fuse(  4, FB, Row, Column) -> fuse_unknown(band005, FB, Row, Column);
fuse(  5, FB, Row, Column) -> fuse_unknown(band006, FB, Row, Column);
fuse(  6, FB, Row, Column) -> fuse_user_upper(FB, Row, Column);
fuse(  7, FB, Row, Column) -> fuse_user_lower(FB, Row, Column);
fuse(  8, FB, Row, Column) -> fuse_unknown(band009, FB, Row, Column);
fuse(  9, FB, Row, Column) -> fuse_unknown(band010, FB, Row, Column);
fuse( 10, FB, Row, Column) -> fuse_unknown(band011, FB, Row, Column);
fuse( 11, FB, Row, Column) -> fuse_unknown(band012, FB, Row, Column);
fuse( 12, FB, Row, Column) -> fuse_feature(pt3_mux0, FB, Row, Column);
fuse( 13, FB, Row, Column) -> fuse_feature(pt3_mux1, FB, Row, Column);
fuse( 14, FB, Row, Column) -> fuse_feature(pt5_mux0, FB, Row, Column);
fuse( 15, FB, Row, Column) -> fuse_feature(pt5_mux1, FB, Row, Column);
fuse( 16, FB, Row, Column) -> fuse_feature(pt4_mux0, FB, Row, Column);
fuse( 17, FB, Row, Column) -> fuse_feature(pt4_mux1, FB, Row, Column);
fuse( 18, FB, Row, Column) -> fuse_feature(pt1_mux0, FB, Row, Column);
fuse( 19, FB, Row, Column) -> fuse_feature(pt1_mux1, FB, Row, Column);
fuse( 20, FB, Row, Column) -> fuse_feature(pt2_mux0, FB, Row, Column);
fuse( 21, FB, Row, Column) -> fuse_feature(pt2_mux1, FB, Row, Column);
fuse( 22, FB, Row, Column) -> fuse_feature(invert, FB, Row, Column);
fuse( 23, FB, Row, Column) -> fuse_feature(from_upper, FB, Row, Column);
fuse( 24, FB, Row, Column) -> fuse_feature(from_lower, FB, Row, Column);
fuse( 25, FB, Row, Column) -> fuse_feature(to_upper, FB, Row, Column);
fuse( 26, FB, Row, Column) -> fuse_feature(std_power, FB, Row, Column);
fuse( 27, FB, Row, Column) -> fuse_feature(oe_gts, FB, Row, Column);
fuse( 28, FB, Row, Column) -> fuse_feature(oe_gts_mux0, FB, Row, Column);
fuse( 29, FB, Row, Column) -> fuse_feature(oe_gts_mux1, FB, Row, Column);
fuse( 30, FB, Row, Column) -> fuse_feature(oe_invert, FB, Row, Column);
fuse( 31, FB, Row, Column) -> fuse_unknown(band032, FB, Row, Column);
fuse( 32, FB, Row, Column) -> fuse_feature(bypass, FB, Row, Column);
fuse( 33, FB, Row, Column) -> fuse_feature(clk_mux0, FB, Row, Column);
fuse( 34, FB, Row, Column) -> fuse_feature(clk_mux1, FB, Row, Column);
fuse( 35, FB, Row, Column) -> fuse_feature(clk_invert, FB, Row, Column);
fuse( 36, FB, Row, Column) -> fuse_feature(ce_or_r, FB, Row, Column);
fuse( 37, FB, Row, Column) -> fuse_feature(ce_or_s, FB, Row, Column);
fuse( 38, FB, Row, Column) -> fuse_unknown(band039, FB, Row, Column);
fuse( 39, FB, Row, Column) -> fuse_feature(t_type, FB, Row, Column);
fuse( 40, FB, Row, Column) -> fuse_feature(r_gsr, FB, Row, Column);
fuse( 41, FB, Row, Column) -> fuse_feature(s_gsr, FB, Row, Column);
fuse( 42, FB, Row, Column) -> fuse_feature(preset, FB, Row, Column);
fuse( 43, FB, Row, Column) -> fuse_feature(ground, FB, Row, Column);
fuse( 44, FB, Row, Column) -> fuse_feature(fast, FB, Row, Column);
fuse( 45, FB, Row, Column) -> fuse_feature(pt3_std_power, FB, Row, Column);
fuse( 46, FB, Row, Column) -> fuse_feature(pt5_std_power, FB, Row, Column);
fuse( 47, FB, Row, Column) -> fuse_feature(pt4_std_power, FB, Row, Column);
fuse( 48, FB, Row, Column) -> fuse_feature(pt1_std_power, FB, Row, Column);
fuse( 49, FB, Row, Column) -> fuse_feature(pt2_std_power, FB, Row, Column);
fuse( 50, FB, Row, Column) -> fuse_input(input01, input28, FB, Row, Column);
fuse( 51, FB, Row, Column) -> fuse_input(input02, input29, FB, Row, Column);
fuse( 52, FB, Row, Column) -> fuse_input(input03, input30, FB, Row, Column);
fuse( 53, FB, Row, Column) -> fuse_input(input04, input31, FB, Row, Column);
fuse( 54, FB, Row, Column) -> fuse_input(input05, input32, FB, Row, Column);
fuse( 55, FB, Row, Column) -> fuse_input(input06, input33, FB, Row, Column);
fuse( 56, FB, Row, Column) -> fuse_input(input07, input34, FB, Row, Column);
fuse( 57, FB, Row, Column) -> fuse_input(input08, input35, FB, Row, Column);
fuse( 58, FB, Row, Column) -> fuse_input(input09, input36, FB, Row, Column);
fuse( 59, FB, Row, Column) -> fuse_input(input10, input37, FB, Row, Column);
fuse( 60, FB, Row, Column) -> fuse_input(input11, input38, FB, Row, Column);
fuse( 61, FB, Row, Column) -> fuse_input(input12, input39, FB, Row, Column);
fuse( 62, FB, Row, Column) -> fuse_input(input13, input40, FB, Row, Column);
fuse( 63, FB, Row, Column) -> fuse_input(input14, input41, FB, Row, Column);
fuse( 64, FB, Row, Column) -> fuse_input(input15, input42, FB, Row, Column);
fuse( 65, FB, Row, Column) -> fuse_input(input16, input43, FB, Row, Column);
fuse( 66, FB, Row, Column) -> fuse_input(input17, input44, FB, Row, Column);
fuse( 67, FB, Row, Column) -> fuse_input(input18, input45, FB, Row, Column);
fuse( 68, FB, Row, Column) -> fuse_input(input19, input46, FB, Row, Column);
fuse( 69, FB, Row, Column) -> fuse_input(input20, input47, FB, Row, Column);
fuse( 70, FB, Row, Column) -> fuse_input(input21, input48, FB, Row, Column);
fuse( 71, FB, Row, Column) -> fuse_input(input22, input49, FB, Row, Column);
fuse( 72, FB, Row, Column) -> fuse_input(input23, input50, FB, Row, Column);
fuse( 73, FB, Row, Column) -> fuse_input(input24, input51, FB, Row, Column);
fuse( 74, FB, Row, Column) -> fuse_input(input25, input52, FB, Row, Column);
fuse( 75, FB, Row, Column) -> fuse_input(input26, input53, FB, Row, Column);
fuse( 76, FB, Row, Column) -> fuse_input(input27, input54, FB, Row, Column);
fuse( 77, FB, Row, Column) -> fuse_unknown(band078, FB, Row, Column);
fuse( 78, FB, Row, Column) -> fuse_function_block(FB, Row, Column);
fuse( 79, FB, Row, Column) -> fuse_unknown(band080, FB, Row, Column);
fuse( 80, FB, Row, Column) -> fuse_unknown(band081, FB, Row, Column);
fuse( 81, FB, Row, Column) -> fuse_unknown(band082, FB, Row, Column);
fuse( 82, FB, Row, Column) -> fuse_unknown(band083, FB, Row, Column);
fuse( 83, FB, Row, Column) -> fuse_unknown(band084, FB, Row, Column);
fuse( 84, FB, Row, Column) -> fuse_unknown(band085, FB, Row, Column);
fuse( 85, FB, Row, Column) -> fuse_unknown(band086, FB, Row, Column);
fuse( 86, FB, Row, Column) -> fuse_unknown(band087, FB, Row, Column);
fuse( 87, FB, Row, Column) -> fuse_unknown(band088, FB, Row, Column);
fuse( 88, FB, Row, Column) -> fuse_unknown(band089, FB, Row, Column);
fuse( 89, FB, Row, Column) -> fuse_unknown(band090, FB, Row, Column);
fuse( 90, FB, Row, Column) -> fuse_unknown(band091, FB, Row, Column);
fuse( 91, FB, Row, Column) -> fuse_unknown(band092, FB, Row, Column);
fuse( 92, FB, Row, Column) -> fuse_unknown(band093, FB, Row, Column);
fuse( 93, FB, Row, Column) -> fuse_unknown(band094, FB, Row, Column);
fuse( 94, FB, Row, Column) -> fuse_unknown(band095, FB, Row, Column);
fuse( 95, FB, Row, Column) -> fuse_unknown(band096, FB, Row, Column);
fuse( 96, FB, Row, Column) -> fuse_unknown(band097, FB, Row, Column);
fuse( 97, FB, Row, Column) -> fuse_unknown(band098, FB, Row, Column);
fuse( 98, FB, Row, Column) -> fuse_unknown(band099, FB, Row, Column);
fuse( 99, FB, Row, Column) -> fuse_unknown(band100, FB, Row, Column);
fuse(100, FB, Row, Column) -> fuse_unknown(band101, FB, Row, Column);
fuse(101, FB, Row, Column) -> fuse_unknown(band102, FB, Row, Column);
fuse(102, FB, Row, Column) -> fuse_unknown(band103, FB, Row, Column);
fuse(103, FB, Row, Column) -> fuse_unknown(band104, FB, Row, Column);
fuse(104, FB, Row, Column) -> fuse_unknown(band105, FB, Row, Column);
fuse(105, FB, Row, Column) -> fuse_unknown(band106, FB, Row, Column);
fuse(106, FB, Row, Column) -> fuse_unknown(band107, FB, Row, Column);
fuse(107, FB, Row, Column) -> fuse_unknown(band108, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_input(Input, _, FB, 0, 6) -> {FB, Input, mux0};
fuse_input(_, Input, FB, 0, 7) -> {FB, Input, mux0};
fuse_input(Input, _, FB, 1, 6) -> {FB, Input, mux1};
fuse_input(_, Input, FB, 1, 7) -> {FB, Input, mux1};
fuse_input(Input, _, FB, 2, 6) -> {FB, Input, mux2};
fuse_input(_, Input, FB, 2, 7) -> {FB, Input, mux2};
fuse_input(Input, _, FB, 3, 6) -> {FB, Input, mux3};
fuse_input(_, Input, FB, 3, 7) -> {FB, Input, mux3};
fuse_input(Input, _, FB, 4, 6) -> {FB, Input, mux4};
fuse_input(_, Input, FB, 4, 7) -> {FB, Input, mux4};
fuse_input(Input, _, FB, 5, 6) -> {FB, Input, mux5};
fuse_input(_, Input, FB, 5, 7) -> {FB, Input, mux5};
fuse_input(Input, _, FB, 6, 6) -> {FB, Input, mux6};
fuse_input(_, Input, FB, 6, 7) -> {FB, Input, mux6};
fuse_input(Input, _, FB, 7, 6) -> {FB, Input, mux7};
fuse_input(_, Input, FB, 7, 7) -> {FB, Input, mux7};
fuse_input(Input, _, FB, 8, 6) -> {FB, Input, mux8};
fuse_input(_, Input, FB, 8, 7) -> {FB, Input, mux8}.

%%--------------------------------------------------------------------

fuse_feature(Feature, FB, Row, Column) ->
    MC = feature_to_macro_cell(Row, Column),
    {FB, MC, Feature}.

%%--------------------------------------------------------------------

fuse_function_block(FB, 0, 6) ->
    {FB, enable};
fuse_function_block(FB, 1, 6) ->
    {FB, forward};
fuse_function_block(FB, 6, 6) ->
    {FB, always};
fuse_function_block(FB, Row, Column) ->
    fuse_unknown(band079, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_global(fb01, 0, 6) -> gsr_invert;
fuse_global(fb01, 1, 6) -> gck1_enable;
fuse_global(fb01, 2, 6) -> gck2_enable;
fuse_global(fb01, 3, 6) -> gck3_enable;
fuse_global(fb01, 4, 6) -> gts1_enable;
fuse_global(fb01, 5, 6) -> gts2_enable;
fuse_global(fb01, 6, 6) -> gts3_enable;
fuse_global(fb01, 7, 6) -> gts4_enable;
fuse_global(fb01, 8, 6) -> keeper_disable;
fuse_global(FB, Row, Column) ->
    fuse_unknown(band003, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_unknown(Band, FB, Row, Column) ->
    MC = feature_to_macro_cell(Row, Column),
    {FB, MC, unknown, Band}.

%%--------------------------------------------------------------------

fuse_user_upper(fb01, 0, 6) -> user30;
fuse_user_upper(fb01, 0, 7) -> user31;
fuse_user_upper(fb01, 1, 6) -> user28;
fuse_user_upper(fb01, 1, 7) -> user29;
fuse_user_upper(fb01, 2, 6) -> user26;
fuse_user_upper(fb01, 2, 7) -> user27;
fuse_user_upper(fb01, 3, 6) -> user24;
fuse_user_upper(fb01, 3, 7) -> user25;
fuse_user_upper(fb01, 4, 6) -> user22;
fuse_user_upper(fb01, 4, 7) -> user23;
fuse_user_upper(fb01, 5, 6) -> user20;
fuse_user_upper(fb01, 5, 7) -> user21;
fuse_user_upper(fb01, 6, 6) -> user18;
fuse_user_upper(fb01, 6, 7) -> user19;
fuse_user_upper(fb01, 7, 6) -> user16;
fuse_user_upper(fb01, 7, 7) -> user17;
fuse_user_upper(FB, Row, Column) ->
    fuse_unknown(band007, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_user_lower(fb01, 0, 6) -> user14;
fuse_user_lower(fb01, 0, 7) -> user15;
fuse_user_lower(fb01, 1, 6) -> user12;
fuse_user_lower(fb01, 1, 7) -> user13;
fuse_user_lower(fb01, 2, 6) -> user10;
fuse_user_lower(fb01, 2, 7) -> user11;
fuse_user_lower(fb01, 3, 6) -> user08;
fuse_user_lower(fb01, 3, 7) -> user09;
fuse_user_lower(fb01, 4, 6) -> user06;
fuse_user_lower(fb01, 4, 7) -> user07;
fuse_user_lower(fb01, 5, 6) -> user04;
fuse_user_lower(fb01, 5, 7) -> user05;
fuse_user_lower(fb01, 6, 6) -> user02;
fuse_user_lower(fb01, 6, 7) -> user03;
fuse_user_lower(fb01, 7, 6) -> user00;
fuse_user_lower(fb01, 7, 7) -> user01;
fuse_user_lower(FB, Row, Column) ->
    fuse_unknown(band008, FB, Row, Column).

%%====================================================================
%% fuses
%%====================================================================

-spec fuses(density(), [fuse()]) -> [name()].

fuses(Density, Fuses) ->
    [ fuse(Density, Fuse) || Fuse <- Fuses ].

%%====================================================================
%% inputs
%%====================================================================

-spec inputs(density(), [fuse()])
    -> #{fb() => #{input() => input:choice()}}.

inputs(Density, Fuses) ->
    inputs(Density, Fuses, #{}).

%%--------------------------------------------------------------------

inputs(_, [], FBs) ->
    FBs;
inputs(Density, [Fuse | Fuses], FBs) ->
    case fuse(Density, Fuse) of
        {FB, Input, mux0} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 1, FBs));

        {FB, Input, mux1} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 2, FBs));

        {FB, Input, mux2} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 4, FBs));

        {FB, Input, mux3} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 8, FBs));

        {FB, Input, mux4} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 16, FBs));

        {FB, Input, mux5} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 32, FBs));

        {FB, Input, mux6} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 64, FBs));

        {FB, Input, mux7} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 128, FBs));

        {FB, Input, mux8} ->
            inputs(Density, Fuses, inputs_add(FB, Input, 256, FBs));

        _ ->
            inputs(Density, Fuses, FBs)
    end.

%%--------------------------------------------------------------------

inputs_add(FB, Input, Bit, FBs) ->
    case FBs of
        #{FB := Inputs = #{Input := Bits}} ->
            FBs#{FB => Inputs#{Input => Bits + Bit}};

        #{FB := Inputs} ->
            FBs#{FB => Inputs#{Input => Bit}};

        _ ->
            FBs#{FB => #{Input => Bit}}
    end.

%%====================================================================
%% name
%%====================================================================

-spec name(density(), name()) -> fuse().

% global fuses
name(Density, gsr_invert) -> name(Density, 2, fb01, 0, 6);
name(Density, gck1_enable) -> name(Density, 2, fb01, 1, 6);
name(Density, gck2_enable) -> name(Density, 2, fb01, 2, 6);
name(Density, gck3_enable) -> name(Density, 2, fb01, 3, 6);
name(Density, gts1_enable) -> name(Density, 2, fb01, 4, 6);
name(Density, gts2_enable) -> name(Density, 2, fb01, 5, 6);
name(Density, gts3_enable) -> name(Density, 2, fb01, 6, 6);
name(Density, gts4_enable) -> name(Density, 2, fb01, 7, 6);
name(Density, keeper_disable) -> name(Density, 2, fb01, 8, 6);
% user upper fuses
name(Density, user30) -> name(Density, 6, fb01, 0, 6);
name(Density, user31) -> name(Density, 6, fb01, 0, 7);
name(Density, user28) -> name(Density, 6, fb01, 1, 6);
name(Density, user29) -> name(Density, 6, fb01, 1, 7);
name(Density, user26) -> name(Density, 6, fb01, 2, 6);
name(Density, user27) -> name(Density, 6, fb01, 2, 7);
name(Density, user24) -> name(Density, 6, fb01, 3, 6);
name(Density, user25) -> name(Density, 6, fb01, 3, 7);
name(Density, user22) -> name(Density, 6, fb01, 4, 6);
name(Density, user23) -> name(Density, 6, fb01, 4, 7);
name(Density, user20) -> name(Density, 6, fb01, 5, 6);
name(Density, user21) -> name(Density, 6, fb01, 5, 7);
name(Density, user18) -> name(Density, 6, fb01, 6, 6);
name(Density, user19) -> name(Density, 6, fb01, 6, 7);
name(Density, user16) -> name(Density, 6, fb01, 7, 6);
name(Density, user17) -> name(Density, 6, fb01, 7, 7);
% user lower fuses
name(Density, user14) -> name(Density, 7, fb01, 0, 6);
name(Density, user15) -> name(Density, 7, fb01, 0, 7);
name(Density, user12) -> name(Density, 7, fb01, 1, 6);
name(Density, user13) -> name(Density, 7, fb01, 1, 7);
name(Density, user10) -> name(Density, 7, fb01, 2, 6);
name(Density, user11) -> name(Density, 7, fb01, 2, 7);
name(Density, user08) -> name(Density, 7, fb01, 3, 6);
name(Density, user09) -> name(Density, 7, fb01, 3, 7);
name(Density, user06) -> name(Density, 7, fb01, 4, 6);
name(Density, user07) -> name(Density, 7, fb01, 4, 7);
name(Density, user04) -> name(Density, 7, fb01, 5, 6);
name(Density, user05) -> name(Density, 7, fb01, 5, 7);
name(Density, user02) -> name(Density, 7, fb01, 6, 6);
name(Density, user03) -> name(Density, 7, fb01, 6, 7);
name(Density, user00) -> name(Density, 7, fb01, 7, 6);
name(Density, user01) -> name(Density, 7, fb01, 7, 7);
% function block fuses
name(Density, {FB, enable}) -> name(Density, 78, FB, 0, 6);
name(Density, {FB, forward}) -> name(Density, 78, FB, 1, 6);
name(Density, {FB, always}) -> name(Density, 78, FB, 6, 6);
% input fuses
name(Density, {FB, Input, mux0}) -> name_input(Density, FB, Input, 0);
name(Density, {FB, Input, mux1}) -> name_input(Density, FB, Input, 1);
name(Density, {FB, Input, mux2}) -> name_input(Density, FB, Input, 2);
name(Density, {FB, Input, mux3}) -> name_input(Density, FB, Input, 3);
name(Density, {FB, Input, mux4}) -> name_input(Density, FB, Input, 4);
name(Density, {FB, Input, mux5}) -> name_input(Density, FB, Input, 5);
name(Density, {FB, Input, mux6}) -> name_input(Density, FB, Input, 6);
name(Density, {FB, Input, mux7}) -> name_input(Density, FB, Input, 7);
name(Density, {FB, Input, mux8}) -> name_input(Density, FB, Input, 8);
% feature fuses
name(Density, {FB, MC, Feature}) ->
    Band = feature_band(Feature),
    {Row, Col} = feature_from_macro_cell(MC),
    name(Density, Band, FB, Row, Col);
name(Density, {FB, MC, unknown, Feature}) ->
    Band = feature_band(Feature),
    {Row, Col} = feature_from_macro_cell(MC),
    name(Density, Band, FB, Row, Col);
% AND-array fuses
name(Density, {FB, MC, PT, Input}) ->
    Band = 1 + (2 * (input:number(Input) - 1)),
    {Row5, Column} = and_array_from_macro_cell(MC),
    Row1 = and_array_from_product_term(PT),
    name(Density, Band, FB, (Row5 * 5) + Row1, Column);
name(Density, {FB, MC, PT, Input, invert}) ->
    Band = 0 + (2 * (input:number(Input) - 1)),
    {Row5, Column} = and_array_from_macro_cell(MC),
    Row1 = and_array_from_product_term(PT),
    name(Density, Band, FB, (Row5 * 5) + Row1, Column).

%%--------------------------------------------------------------------

name(Density, Band, FB_, Row, Column) when Row < 9 ->
    % wide row
    FBCount = density:function_block_count(Density),
    BandSize = FBCount * 108,
    WideRowSize = FBCount * 8,
    %
    FB = function_block:number(FB_) - 1,
    (Band * BandSize) +
    (Row * WideRowSize) +
    (FB * 8) +
    Column;
name(Density, Band, FB_, Row_, Column) ->
    % slim row
    FBCount = density:function_block_count(Density),
    BandSize = FBCount * 108,
    WideRowSize = FBCount * 8,
    SlimRowSize = FBCount * 6,
    %
    FB = function_block:number(FB_) - 1,
    Row = Row_ - 9,
    (Band * BandSize) +
    (9 * WideRowSize) +
    (Row * SlimRowSize) +
    (FB * 6) +
    Column.

%%--------------------------------------------------------------------

name_input(Density, FB, input01, Row) -> name(Density, 50, FB, Row, 6);
name_input(Density, FB, input02, Row) -> name(Density, 51, FB, Row, 6);
name_input(Density, FB, input03, Row) -> name(Density, 52, FB, Row, 6);
name_input(Density, FB, input04, Row) -> name(Density, 53, FB, Row, 6);
name_input(Density, FB, input05, Row) -> name(Density, 54, FB, Row, 6);
name_input(Density, FB, input06, Row) -> name(Density, 55, FB, Row, 6);
name_input(Density, FB, input07, Row) -> name(Density, 56, FB, Row, 6);
name_input(Density, FB, input08, Row) -> name(Density, 57, FB, Row, 6);
name_input(Density, FB, input09, Row) -> name(Density, 58, FB, Row, 6);
name_input(Density, FB, input10, Row) -> name(Density, 59, FB, Row, 6);
name_input(Density, FB, input11, Row) -> name(Density, 60, FB, Row, 6);
name_input(Density, FB, input12, Row) -> name(Density, 61, FB, Row, 6);
name_input(Density, FB, input13, Row) -> name(Density, 62, FB, Row, 6);
name_input(Density, FB, input14, Row) -> name(Density, 63, FB, Row, 6);
name_input(Density, FB, input15, Row) -> name(Density, 64, FB, Row, 6);
name_input(Density, FB, input16, Row) -> name(Density, 65, FB, Row, 6);
name_input(Density, FB, input17, Row) -> name(Density, 66, FB, Row, 6);
name_input(Density, FB, input18, Row) -> name(Density, 67, FB, Row, 6);
name_input(Density, FB, input19, Row) -> name(Density, 68, FB, Row, 6);
name_input(Density, FB, input20, Row) -> name(Density, 69, FB, Row, 6);
name_input(Density, FB, input21, Row) -> name(Density, 70, FB, Row, 6);
name_input(Density, FB, input22, Row) -> name(Density, 71, FB, Row, 6);
name_input(Density, FB, input23, Row) -> name(Density, 72, FB, Row, 6);
name_input(Density, FB, input24, Row) -> name(Density, 73, FB, Row, 6);
name_input(Density, FB, input25, Row) -> name(Density, 74, FB, Row, 6);
name_input(Density, FB, input26, Row) -> name(Density, 75, FB, Row, 6);
name_input(Density, FB, input27, Row) -> name(Density, 76, FB, Row, 6);
name_input(Density, FB, input28, Row) -> name(Density, 50, FB, Row, 7);
name_input(Density, FB, input29, Row) -> name(Density, 51, FB, Row, 7);
name_input(Density, FB, input30, Row) -> name(Density, 52, FB, Row, 7);
name_input(Density, FB, input31, Row) -> name(Density, 53, FB, Row, 7);
name_input(Density, FB, input32, Row) -> name(Density, 54, FB, Row, 7);
name_input(Density, FB, input33, Row) -> name(Density, 55, FB, Row, 7);
name_input(Density, FB, input34, Row) -> name(Density, 56, FB, Row, 7);
name_input(Density, FB, input35, Row) -> name(Density, 57, FB, Row, 7);
name_input(Density, FB, input36, Row) -> name(Density, 58, FB, Row, 7);
name_input(Density, FB, input37, Row) -> name(Density, 59, FB, Row, 7);
name_input(Density, FB, input38, Row) -> name(Density, 60, FB, Row, 7);
name_input(Density, FB, input39, Row) -> name(Density, 61, FB, Row, 7);
name_input(Density, FB, input40, Row) -> name(Density, 62, FB, Row, 7);
name_input(Density, FB, input41, Row) -> name(Density, 63, FB, Row, 7);
name_input(Density, FB, input42, Row) -> name(Density, 64, FB, Row, 7);
name_input(Density, FB, input43, Row) -> name(Density, 65, FB, Row, 7);
name_input(Density, FB, input44, Row) -> name(Density, 66, FB, Row, 7);
name_input(Density, FB, input45, Row) -> name(Density, 67, FB, Row, 7);
name_input(Density, FB, input46, Row) -> name(Density, 68, FB, Row, 7);
name_input(Density, FB, input47, Row) -> name(Density, 69, FB, Row, 7);
name_input(Density, FB, input48, Row) -> name(Density, 70, FB, Row, 7);
name_input(Density, FB, input49, Row) -> name(Density, 71, FB, Row, 7);
name_input(Density, FB, input50, Row) -> name(Density, 72, FB, Row, 7);
name_input(Density, FB, input51, Row) -> name(Density, 73, FB, Row, 7);
name_input(Density, FB, input52, Row) -> name(Density, 74, FB, Row, 7);
name_input(Density, FB, input53, Row) -> name(Density, 75, FB, Row, 7);
name_input(Density, FB, input54, Row) -> name(Density, 76, FB, Row, 7).

%%--------------------------------------------------------------------

feature_band(band001) ->   0;
feature_band(band002) ->   1;
feature_band(band003) ->   2;
feature_band(band004) ->   3;
feature_band(band005) ->   4;
feature_band(band006) ->   5;
feature_band(band007) ->   6;
feature_band(band008) ->   7;
feature_band(band009) ->   8;
feature_band(band010) ->   9;
feature_band(band011) ->  10;
feature_band(band012) ->  11;
feature_band(pt3_mux0) ->  12;
feature_band(pt3_mux1) ->  13;
feature_band(pt5_mux0) ->  14;
feature_band(pt5_mux1) ->  15;
feature_band(pt4_mux0) ->  16;
feature_band(pt4_mux1) ->  17;
feature_band(pt1_mux0) ->  18;
feature_band(pt1_mux1) ->  19;
feature_band(pt2_mux0) ->  20;
feature_band(pt2_mux1) ->  21;
feature_band(invert) ->  22;
feature_band(from_upper) ->  23;
feature_band(from_lower) ->  24;
feature_band(to_upper) ->  25;
feature_band(std_power) ->  26;
feature_band(oe_gts) ->  27;
feature_band(oe_gts_mux0) ->  28;
feature_band(oe_gts_mux1) ->  29;
feature_band(oe_invert) ->  30;
feature_band(band032) ->  31;
feature_band(bypass) ->  32;
feature_band(clk_mux0) ->  33;
feature_band(clk_mux1) ->  34;
feature_band(clk_invert) ->  35;
feature_band(ce_or_r) ->  36;
feature_band(ce_or_s) ->  37;
feature_band(band039) ->  38;
feature_band(t_type) ->  39;
feature_band(r_gsr) ->  40;
feature_band(s_gsr) ->  41;
feature_band(preset) ->  42;
feature_band(ground) ->  43;
feature_band(fast) ->  44;
feature_band(pt3_std_power) ->  45;
feature_band(pt5_std_power) ->  46;
feature_band(pt4_std_power) ->  47;
feature_band(pt1_std_power) ->  48;
feature_band(pt2_std_power) ->  49;
feature_band(band051) ->  50;
feature_band(band052) ->  51;
feature_band(band053) ->  52;
feature_band(band054) ->  53;
feature_band(band055) ->  54;
feature_band(band056) ->  55;
feature_band(band057) ->  56;
feature_band(band058) ->  57;
feature_band(band059) ->  58;
feature_band(band060) ->  59;
feature_band(band061) ->  60;
feature_band(band062) ->  61;
feature_band(band063) ->  62;
feature_band(band064) ->  63;
feature_band(band065) ->  64;
feature_band(band066) ->  65;
feature_band(band067) ->  66;
feature_band(band068) ->  67;
feature_band(band069) ->  68;
feature_band(band070) ->  69;
feature_band(band071) ->  70;
feature_band(band072) ->  71;
feature_band(band073) ->  72;
feature_band(band074) ->  73;
feature_band(band075) ->  74;
feature_band(band076) ->  75;
feature_band(band077) ->  76;
feature_band(band078) ->  77;
feature_band(band079) ->  78;
feature_band(band080) ->  79;
feature_band(band081) ->  80;
feature_band(band082) ->  81;
feature_band(band083) ->  82;
feature_band(band084) ->  83;
feature_band(band085) ->  84;
feature_band(band086) ->  85;
feature_band(band087) ->  86;
feature_band(band088) ->  87;
feature_band(band089) ->  88;
feature_band(band090) ->  89;
feature_band(band091) ->  90;
feature_band(band092) ->  91;
feature_band(band093) ->  92;
feature_band(band094) ->  93;
feature_band(band095) ->  94;
feature_band(band096) ->  95;
feature_band(band097) ->  96;
feature_band(band098) ->  97;
feature_band(band099) ->  98;
feature_band(band100) ->  99;
feature_band(band101) -> 100;
feature_band(band102) -> 101;
feature_band(band103) -> 102;
feature_band(band104) -> 103;
feature_band(band105) -> 104;
feature_band(band106) -> 105;
feature_band(band107) -> 106;
feature_band(band108) -> 107.

