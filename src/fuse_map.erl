-module(fuse_map).

-export([fuse/2]).
-export([fuses/2]).

-type density() :: density:density().
-type fb() :: function_block:function_block().
-type input() :: input:input().
-type mc() :: macro_cell:macro_cell().
-type pt() :: product_term:product_term().
-type user() :: fuse:user().

-type fuse() :: non_neg_integer().
-type feature() ::
    bypass |
    fast |
    gck_0 |
    gck_1 |
    ground |
    oe_invert |
    oe_gts |
    oe_gts_0 |
    oe_gts_1 |
    preset |
    r_gsr |
    s_gsr |
    t_type.

%%====================================================================
%% and_array
%%====================================================================

and_array_macro_cell(0, 0) -> mc01;
and_array_macro_cell(0, 1) -> mc04;
and_array_macro_cell(0, 2) -> mc07;
and_array_macro_cell(0, 3) -> mc10;
and_array_macro_cell(0, 4) -> mc13;
and_array_macro_cell(0, 5) -> mc16;
and_array_macro_cell(1, 0) -> mc02;
and_array_macro_cell(1, 1) -> mc05;
and_array_macro_cell(1, 2) -> mc08;
and_array_macro_cell(1, 3) -> mc11;
and_array_macro_cell(1, 4) -> mc14;
and_array_macro_cell(1, 5) -> mc17;
and_array_macro_cell(2, 0) -> mc03;
and_array_macro_cell(2, 1) -> mc06;
and_array_macro_cell(2, 2) -> mc09;
and_array_macro_cell(2, 3) -> mc12;
and_array_macro_cell(2, 4) -> mc15;
and_array_macro_cell(2, 5) -> mc18.

%%--------------------------------------------------------------------

and_array_product_term(0) -> pt3;
and_array_product_term(1) -> pt5;
and_array_product_term(2) -> pt4;
and_array_product_term(3) -> pt1;
and_array_product_term(4) -> pt2.

%%====================================================================
%% feature
%%====================================================================

feature_macro_cell(0, 6) ->mc01;
feature_macro_cell(1, 6) ->mc02;
feature_macro_cell(2, 6) ->mc03;
feature_macro_cell(3, 6) ->mc04;
feature_macro_cell(4, 6) ->mc05;
feature_macro_cell(5, 6) ->mc06;
feature_macro_cell(6, 6) ->mc07;
feature_macro_cell(7, 6) ->mc08;
feature_macro_cell(8, 6) ->mc09;
feature_macro_cell(0, 7) ->mc10;
feature_macro_cell(1, 7) ->mc11;
feature_macro_cell(2, 7) ->mc12;
feature_macro_cell(3, 7) ->mc13;
feature_macro_cell(4, 7) ->mc14;
feature_macro_cell(5, 7) ->mc15;
feature_macro_cell(6, 7) ->mc16;
feature_macro_cell(7, 7) ->mc17;
feature_macro_cell(8, 7) ->mc18.

%%====================================================================
%% fuse
%%====================================================================

-spec fuse(density(), fuse())
    -> gsr_invert |
       user() |
       {fb(), mc(), pt(), input()} |
       {fb(), mc(), pt(), input()} |
       {fb(), mc(), feature()} |
       undefined.

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
    MC = and_array_macro_cell(Row div 5, Column),
    PT = and_array_product_term(Row rem 5),
    Input = input:from((Band div 2) + 1),
    case Band rem 2 of
        0 ->
            {FB, MC, PT, Input, invert};

        1 ->
            {FB, MC, PT, Input}
    end;
fuse(  0, FB, Row, Column) -> fuse_unknown(band001, FB, Row, Column);
fuse(  1, FB, Row, Column) -> fuse_unknown(band002, FB, Row, Column);
fuse(  2, FB, Row, Column) -> fuse_gsr_invert(FB, Row, Column);
fuse(  3, FB, Row, Column) -> fuse_unknown(band004, FB, Row, Column);
fuse(  4, FB, Row, Column) -> fuse_unknown(band005, FB, Row, Column);
fuse(  5, FB, Row, Column) -> fuse_unknown(band006, FB, Row, Column);
fuse(  6, FB, Row, Column) -> fuse_user_upper(FB, Row, Column);
fuse(  7, FB, Row, Column) -> fuse_user_lower(FB, Row, Column);
fuse(  8, FB, Row, Column) -> fuse_unknown(band009, FB, Row, Column);
fuse(  9, FB, Row, Column) -> fuse_unknown(band010, FB, Row, Column);
fuse( 10, FB, Row, Column) -> fuse_unknown(band011, FB, Row, Column);
fuse( 11, FB, Row, Column) -> fuse_unknown(band012, FB, Row, Column);
fuse( 12, FB, Row, Column) -> fuse_unknown(band013, FB, Row, Column);
fuse( 13, FB, Row, Column) -> fuse_unknown(pt5_2, FB, Row, Column);
fuse( 14, FB, Row, Column) -> fuse_unknown(pt5_1, FB, Row, Column);
fuse( 15, FB, Row, Column) -> fuse_unknown(pt5_0, FB, Row, Column);
fuse( 16, FB, Row, Column) -> fuse_unknown(pt4_1, FB, Row, Column);
fuse( 17, FB, Row, Column) -> fuse_unknown(pt4_0, FB, Row, Column);
fuse( 18, FB, Row, Column) -> fuse_unknown(pt1_1, FB, Row, Column);
fuse( 19, FB, Row, Column) -> fuse_unknown(pt1_0, FB, Row, Column);
fuse( 20, FB, Row, Column) -> fuse_unknown(pt2_1, FB, Row, Column);
fuse( 21, FB, Row, Column) -> fuse_unknown(pt2_0, FB, Row, Column);
fuse( 22, FB, Row, Column) -> fuse_unknown(xor1, FB, Row, Column);
fuse( 23, FB, Row, Column) -> fuse_unknown(band024, FB, Row, Column);
fuse( 24, FB, Row, Column) -> fuse_unknown(band025, FB, Row, Column);
fuse( 25, FB, Row, Column) -> fuse_unknown(band026, FB, Row, Column);
fuse( 26, FB, Row, Column) -> fuse_unknown(band027, FB, Row, Column);
fuse( 27, FB, Row, Column) -> fuse_feature(oe_gts, FB, Row, Column);
fuse( 28, FB, Row, Column) -> fuse_feature(oe_gts_0, FB, Row, Column);
fuse( 29, FB, Row, Column) -> fuse_feature(oe_gts_1, FB, Row, Column);
fuse( 30, FB, Row, Column) -> fuse_feature(oe_invert, FB, Row, Column);
fuse( 31, FB, Row, Column) -> fuse_unknown(band032, FB, Row, Column);
fuse( 32, FB, Row, Column) -> fuse_feature(bypass, FB, Row, Column);
fuse( 33, FB, Row, Column) -> fuse_feature(gck_1, FB, Row, Column);
fuse( 34, FB, Row, Column) -> fuse_feature(gck_0, FB, Row, Column);
fuse( 35, FB, Row, Column) -> fuse_unknown(band036, FB, Row, Column);
fuse( 36, FB, Row, Column) -> fuse_unknown(ce_1, FB, Row, Column);
fuse( 37, FB, Row, Column) -> fuse_unknown(ce_0, FB, Row, Column);
fuse( 38, FB, Row, Column) -> fuse_unknown(band039, FB, Row, Column);
fuse( 39, FB, Row, Column) -> fuse_feature(t_type, FB, Row, Column);
fuse( 40, FB, Row, Column) -> fuse_feature(r_gsr, FB, Row, Column);
fuse( 41, FB, Row, Column) -> fuse_feature(s_gsr, FB, Row, Column);
fuse( 42, FB, Row, Column) -> fuse_feature(preset, FB, Row, Column);
fuse( 43, FB, Row, Column) -> fuse_feature(ground, FB, Row, Column);
fuse( 44, FB, Row, Column) -> fuse_feature(fast, FB, Row, Column);
fuse( 45, FB, Row, Column) -> fuse_unknown(band046, FB, Row, Column);
fuse( 46, FB, Row, Column) -> fuse_unknown(pt5, FB, Row, Column);
fuse( 47, FB, Row, Column) -> fuse_unknown(pt4, FB, Row, Column);
fuse( 48, FB, Row, Column) -> fuse_unknown(pt1, FB, Row, Column);
fuse( 49, FB, Row, Column) -> fuse_unknown(pt2, FB, Row, Column);
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
fuse( 78, FB, Row, Column) -> fuse_unknown(band079, FB, Row, Column);
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

fuse_input(Input, _, FB, 0, 6) -> {FB, Input, mux_0};
fuse_input(_, Input, FB, 0, 7) -> {FB, Input, mux_0};
fuse_input(Input, _, FB, 1, 6) -> {FB, Input, mux_1};
fuse_input(_, Input, FB, 1, 7) -> {FB, Input, mux_1};
fuse_input(Input, _, FB, 2, 6) -> {FB, Input, mux_2};
fuse_input(_, Input, FB, 2, 7) -> {FB, Input, mux_2};
fuse_input(Input, _, FB, 3, 6) -> {FB, Input, mux_3};
fuse_input(_, Input, FB, 3, 7) -> {FB, Input, mux_3};
fuse_input(Input, _, FB, 4, 6) -> {FB, Input, mux_4};
fuse_input(_, Input, FB, 4, 7) -> {FB, Input, mux_4};
fuse_input(Input, _, FB, 5, 6) -> {FB, Input, mux_5};
fuse_input(_, Input, FB, 5, 7) -> {FB, Input, mux_5};
fuse_input(Input, _, FB, 6, 6) -> {FB, Input, mux_6};
fuse_input(_, Input, FB, 6, 7) -> {FB, Input, mux_6};
fuse_input(Input, _, FB, 7, 6) -> {FB, Input, mux_7};
fuse_input(_, Input, FB, 7, 7) -> {FB, Input, mux_7};
fuse_input(Input, _, FB, 8, 6) -> {FB, Input, mux_8};
fuse_input(_, Input, FB, 8, 7) -> {FB, Input, mux_8}.

%%--------------------------------------------------------------------

fuse_feature(Feature, FB, Row, Column) ->
    MC = feature_macro_cell(Row, Column),
    {FB, MC, Feature}.

%%--------------------------------------------------------------------

fuse_gsr_invert(fb01, 0, 6) ->
    gsr_invert;
fuse_gsr_invert(FB, Row, Column) ->
    fuse_unknown(band03, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_unknown(Band, FB, Row, Column) ->
    MC = feature_macro_cell(Row, Column),
    {FB, MC, unknown, Band}.

%%--------------------------------------------------------------------

fuse_user_upper(fb01, 0, 6) -> user30;
fuse_user_upper(fb01, 0, 7) -> user31;
fuse_user_upper(fb01, 1, 6) -> user28;
fuse_user_upper(fb01, 1, 7) -> user29;
fuse_user_upper(fb01, 2, 6) -> user25;
fuse_user_upper(fb01, 2, 7) -> user24;
fuse_user_upper(fb01, 3, 6) -> user27;
fuse_user_upper(fb01, 3, 7) -> user26;
fuse_user_upper(fb01, 4, 6) -> user22;
fuse_user_upper(fb01, 4, 7) -> user23;
fuse_user_upper(fb01, 5, 6) -> user20;
fuse_user_upper(fb01, 5, 7) -> user21;
fuse_user_upper(fb01, 6, 6) -> user17;
fuse_user_upper(fb01, 6, 7) -> user16;
fuse_user_upper(fb01, 7, 6) -> user19;
fuse_user_upper(fb01, 7, 7) -> user18;
fuse_user_upper(FB, Row, Column) ->
    fuse_unknown(band07, FB, Row, Column).

%%--------------------------------------------------------------------

fuse_user_lower(fb01, 0, 6) -> user14;
fuse_user_lower(fb01, 0, 7) -> user15;
fuse_user_lower(fb01, 1, 6) -> user12;
fuse_user_lower(fb01, 1, 7) -> user13;
fuse_user_lower(fb01, 2, 6) -> user09;
fuse_user_lower(fb01, 2, 7) -> user08;
fuse_user_lower(fb01, 3, 6) -> user11;
fuse_user_lower(fb01, 3, 7) -> user10;
fuse_user_lower(fb01, 4, 6) -> user06;
fuse_user_lower(fb01, 4, 7) -> user07;
fuse_user_lower(fb01, 5, 6) -> user04;
fuse_user_lower(fb01, 5, 7) -> user05;
fuse_user_lower(fb01, 6, 6) -> user01;
fuse_user_lower(fb01, 6, 7) -> user00;
fuse_user_lower(fb01, 7, 6) -> user03;
fuse_user_lower(fb01, 7, 7) -> user02;
fuse_user_lower(FB, Row, Column) ->
    fuse_unknown(band07, FB, Row, Column).

%%====================================================================
%% fuses
%%====================================================================

fuses(Density, Fuses) ->
    [ fuse(Density, Fuse) || Fuse <- Fuses ].

