-module(jed_to_vhdl).

-export([run/0]).
-export([run/2]).
-export([run/3]).

%%====================================================================
%% run
%%====================================================================

run() ->
    run("sample.jed", "sample").

%%--------------------------------------------------------------------

run(JEDFile, VHDLFile) ->
    run(xc95288xl_tq144, JEDFile, VHDLFile).

%%--------------------------------------------------------------------

run(Device, JEDFile, WriteTo) ->
    write_to(WriteTo),
    {UCF, VHDL} = internal(Device, JEDFile),
    write_to(UCF, WriteTo, "experiment.ucf"),
    write_to(VHDL, WriteTo, "experiment.vhd"),
    ok.

%%--------------------------------------------------------------------

write_to(screen) ->
    ok;
write_to(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;

        {error, eexist} ->
            ok
    end.

%%--------------------------------------------------------------------

write_to(Output, screen, _FileName) ->
    io:format("~s", [Output]);
write_to(Output, Dir, FileName) ->
    File = filename:join(Dir, FileName),
    ok = file:write_file(File, Output).

%%====================================================================
%% internal
%%====================================================================

internal(Device, JEDFile) ->
    Fuses = experiment:jed_file(JEDFile),
    Density = device:density(Device),
    Collect = collect(Device, Density, Fuses),
    Names = names(Density, Collect),
    Inputs = inputs(Density, Collect, Names),
    Cells = compile(Collect, Inputs, Names),
    UCF = ucf(Names),
    VHDL = vhdl(Cells, Names),
    {UCF, VHDL}.

%%====================================================================
%% collect
%%====================================================================

collect(Device, Density, Fuses) ->
    PinNames = maps:from_list([
        {Pin, Name}
        ||
        {Name, Pin} <- device:io_pins(Device)
    ]),
    GCKs = device:gck_macro_cells(Device),
    GSR = device:gsr_macro_cell(Device),
    GTSs = device:gts_macro_cells(Device),
    Collect0 = #{
        global => #{
            gcks => GCKs,
            gsr => GSR,
            gtss => GTSs,
            pins => PinNames
        }
    },
    lists:foldl(fun (Fuse, Collect) ->
        collect_fuse(fuse_map:fuse(Density, Fuse), Collect)
    end, Collect0, Fuses).

%%--------------------------------------------------------------------

collect_fuse(undefined, Collect) -> Collect;
collect_fuse(user00, Collect) -> Collect;
collect_fuse(user01, Collect) -> Collect;
collect_fuse(user02, Collect) -> Collect;
collect_fuse(user03, Collect) -> Collect;
collect_fuse(user04, Collect) -> Collect;
collect_fuse(user05, Collect) -> Collect;
collect_fuse(user06, Collect) -> Collect;
collect_fuse(user07, Collect) -> Collect;
collect_fuse(user08, Collect) -> Collect;
collect_fuse(user09, Collect) -> Collect;
collect_fuse(user10, Collect) -> Collect;
collect_fuse(user11, Collect) -> Collect;
collect_fuse(user12, Collect) -> Collect;
collect_fuse(user13, Collect) -> Collect;
collect_fuse(user14, Collect) -> Collect;
collect_fuse(user15, Collect) -> Collect;
collect_fuse(user16, Collect) -> Collect;
collect_fuse(user17, Collect) -> Collect;
collect_fuse(user18, Collect) -> Collect;
collect_fuse(user19, Collect) -> Collect;
collect_fuse(user20, Collect) -> Collect;
collect_fuse(user21, Collect) -> Collect;
collect_fuse(user22, Collect) -> Collect;
collect_fuse(user23, Collect) -> Collect;
collect_fuse(user24, Collect) -> Collect;
collect_fuse(user25, Collect) -> Collect;
collect_fuse(user26, Collect) -> Collect;
collect_fuse(user27, Collect) -> Collect;
collect_fuse(user28, Collect) -> Collect;
collect_fuse(user29, Collect) -> Collect;
collect_fuse(user30, Collect) -> Collect;
collect_fuse(user31, Collect) -> Collect;
collect_fuse(Feature, Collect) when is_atom(Feature) ->
    collect_inside(Collect, global, fun (Global) ->
        collect_yes(Global, Feature)
    end);
collect_fuse({FB, Feature}, Collect) ->
    collect_inside(Collect, FB, fun (Block) ->
        collect_yes(Block, Feature)
    end);
collect_fuse({FB, Input, Mux = mux0}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux1}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux2}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux3}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux4}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux5}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux6}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux7}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, Input, Mux = mux8}, Collect) ->
    collect_input(Collect, FB, Input, Mux);
collect_fuse({FB, MC, Feature}, Collect) ->
    collect_cell(Collect, FB, MC, Feature);
collect_fuse({FB, MC, PT, Input}, Collect) ->
    collect_term(Collect, FB, MC, PT, Input);
collect_fuse({FB, MC, PT, Input, invert}, Collect) ->
    collect_term(Collect, FB, MC, PT, {invert, Input}).

%%--------------------------------------------------------------------

collect_yes(Map, Feature) ->
    Map#{Feature => yes}.

%%--------------------------------------------------------------------

collect_inside(Map, Key, Fun) ->
    case Map of
        #{Key := Value0} ->
            Value = Fun(Value0),
            Map#{Key => Value};

        _ ->
            Value = Fun(#{}),
            Map#{Key => Value}
    end.

%%--------------------------------------------------------------------

collect_push(Map, Key, Item) ->
    case Map of
        #{Key := Items} ->
            Map#{Key => [Item | Items]};

        _ ->
            Map#{Key => [Item]}
    end.

%%--------------------------------------------------------------------

collect_input(Collect, FB, Input, Mux) ->
    collect_push(Collect, {FB, input, Input}, Mux).

%%--------------------------------------------------------------------

collect_cell(Collect, FB, MC, Feature) ->
    collect_inside(Collect, {FB, cell, MC}, fun (Cell) ->
        collect_yes(Cell, Feature)
    end).

%%--------------------------------------------------------------------

collect_term(Collect, FB, MC, PT, Input) ->
    collect_inside(Collect, {FB, cell, MC}, fun (Cell) ->
        collect_push(Cell, PT, Input)
    end).

%%====================================================================
%% choice
%%====================================================================

choice([]) -> 0;
choice([mux0]) -> 1;
choice([mux1]) -> 2;
choice([mux1,mux0]) -> 3;
choice([mux2]) -> 4;
choice([mux2,mux0]) -> 5;
choice([mux2,mux1]) -> 6;
choice([mux2,mux1,mux0]) -> 7;
choice([mux3]) -> 8;
choice([mux3,mux0]) -> 9;
choice([mux3,mux1]) -> 10;
choice([mux3,mux1,mux0]) -> 11;
choice([mux3,mux2]) -> 12;
choice([mux3,mux2,mux0]) -> 13;
choice([mux3,mux2,mux1]) -> 14;
choice([mux3,mux2,mux1,mux0]) -> 15;
choice([mux4]) -> 16;
choice([mux4,mux0]) -> 17;
choice([mux4,mux1]) -> 18;
choice([mux4,mux1,mux0]) -> 19;
choice([mux4,mux2]) -> 20;
choice([mux4,mux2,mux0]) -> 21;
choice([mux4,mux2,mux1]) -> 22;
choice([mux4,mux2,mux1,mux0]) -> 23;
choice([mux4,mux3]) -> 24;
choice([mux4,mux3,mux0]) -> 25;
choice([mux4,mux3,mux1]) -> 26;
choice([mux4,mux3,mux1,mux0]) -> 27;
choice([mux4,mux3,mux2]) -> 28;
choice([mux4,mux3,mux2,mux0]) -> 29;
choice([mux4,mux3,mux2,mux1]) -> 30;
choice([mux4,mux3,mux2,mux1,mux0]) -> 31.

%%====================================================================
%% oe
%%====================================================================

oe(Cell = #{pt5_mux0 := yes, pt5_mux1 := yes, pt5 := Terms}) ->
    false = is_map_key(oe_gts, Cell),
    case Cell of
        #{oe_invert := yes} ->
            {invert, Terms};

        _ ->
            Terms
    end;
oe(Cell = #{oe_gts := yes}) ->
    GTS = case Cell of
        #{oe_gts_mux1 := yes, oe_gts_mux0 := yes} -> gts4;
        #{oe_gts_mux1 := yes} -> gts3;
        #{oe_gts_mux0 := yes} -> gts2;
        _-> gts1
    end,
    case Cell of
        #{oe_invert := yes} ->
            {invert, GTS};

        _ ->
            GTS
    end;
oe(#{oe_invert := yes}) ->
    always;
oe(_) ->
    never.

%%====================================================================
%% names
%%====================================================================

% {_, external} pins get named "mc<FB>_<MC>_<PIN>"
% {_, internal} cells get renamed to {_, external} if OE = always
% GSR, CCK#, GTS# get renamed to "gsr_<PIN>", .. when used as globals

% Generate a map
% #{ {mc(), external} => {binary(), dir()},
%    {mc(), internal} => binary()
% }

names(Density, Collect = #{global := Global}) ->
    #{pins := PinNames} = Global,
    Names0 = input_names(Density, Collect),
    Names1 = vhdl_names(Collect, Names0),
    Names2 = gck_names(Global, Collect, Names1),
    Names3 = gsr_name(Global, Collect, Names2),
    Names4 = gts_names(Global, Collect, Names3),
    Names5 = maps:map(fun (Key, Value) ->
        external_name(Key, Value, PinNames)
    end, Names4),
    Names6 = maps:map(fun (Key, Value) ->
        internal_name(Key, Value, Names5)
    end, Names5),
    Names7 = maps:map(fun (Key, Value) ->
        logic_name(Key, Value, Names6)
    end, Names6),
    Names7.

%%--------------------------------------------------------------------

input_names(Density, Collect) ->
    maps:fold(fun (Key, Value, Names) ->
        input_name(Density, Key, Value, Names)
    end, #{}, Collect).

%%--------------------------------------------------------------------

input_name(Density, {_FB, input, Input}, Muxes, Names) ->
    Source = input_map:choice(Density, Input, choice(Muxes)),
    case Source of
        {_, external} ->
            Names#{Source => in};

        {_, internal} ->
            Names#{Source => internal}
    end;
input_name(_Density, _Key, _Value, Names) ->
    Names.

%%--------------------------------------------------------------------

vhdl_names(Collect, Names0) ->
    maps:fold(fun (Key, Value, Names) ->
        vhdl_name(Key, Value, Names)
    end, Names0, Collect).

%%--------------------------------------------------------------------

vhdl_name({FB, cell, MC}, Cell, Names) ->
    %
    % See /jed_to_vhdl/types/source/experiment.vhd
    % for explanation of types
    %
    case oe(Cell) of
        never ->
            Pin = macro_cell:join(FB, MC),
            case Names of
                #{{Pin, external} := in, {Pin, internal} := internal} ->
                    % type 3 - input AND logic
                    % input
                    %   ... <= ... mc_pin ...
                    % AND logic
                    %   mc <= ...;
                    %   ... <= ... mc ...
                    Names#{
                        {Pin, logic} => internal
                    };

                #{{Pin, external} := in} ->
                    % type 2 - input ONLY
                    %   ... <= ... mc_pin ...
                    Names;

                #{{Pin, internal} := internal} ->
                    % type 1 - logic ONLY
                    %   mc <= ...;
                    %   ... <= ... mc ...
                    Names#{
                        {Pin, logic} => internal
                    };

                _ ->
                    % Cell just forwards
                    Names
            end;

        always ->
            Pin = macro_cell:join(FB, MC),
            case Names of
                #{{Pin, external} := in} ->
                    % output ALWAYS, why read from external?
                    throw({output, always, read, from, external});

                #{{Pin, internal} := internal} ->
                    % type 6 - output USED internally
                    %   mc <= ...;
                    %   mc_pin <= mc;
                    %   ... <= ... mc ...
                    Names#{
                        {Pin, external} => out,
                        {Pin, internal} => internal,
                        {Pin, logic} => external,
                        {Pin, pin} => external
                    };

                _ ->
                    % type 5 - output ONLY
                    %   mc_pin <= ...;
                    Names#{
                        {Pin, external} => out,
                        {Pin, logic} => external
                    }
            end;

        _ ->
            Pin = macro_cell:join(FB, MC),
            case Names of
                #{{Pin, external} := in} ->
                    % type 9 - input AND oe ONLY
                    % type 10 - input AND oe USED internally
                    %   mc_ibuf : IBUF port map (
                    %     I => mc_pin,
                    %     O => mc_in
                    %   );
                    %   mc <= ...;
                    %   mc_obuf : OBUFE port map (
                    %     I >= mc,
                    %     O => mc_pin,
                    %     E => ...
                    %   );
                    %   ... <= ... mc_in ...
                    %   ... <= ... mc ...
                    Names#{
                        {Pin, external} => inout,
                        {Pin, logic} => external,
                        {Pin, pin} => external
                    };

                _ ->
                    % type 7 - oe ONLY
                    % type 8 - oe USED internally
                    %   mc <= ...;
                    %   mc_obuf : OBUFE port map (
                    %     I => mc,
                    %     O => mc_pin,
                    %     E => ...
                    %   );
                    %   ... <= ... mc ...
                    Names#{
                        {Pin, external} => out,
                        {Pin, logic} => external
                    }
            end
    end;
vhdl_name(_Key, _Value, Names) ->
    Names.

%%--------------------------------------------------------------------

global_name(true, MC, Global, BufG, Names) ->
    case Names of
        #{{MC, external} := Dir} ->
            in = Dir,
            Names#{{MC, external} => {Global, BufG}};

        _ ->
            Names#{{MC, external} => {Global, BufG}}
    end;
global_name(false, _MC, _Global, _BufG, Names) ->
    Names.

%%--------------------------------------------------------------------

gck_names(Global, Collect, Names0) ->
    {Used1, Used2, Used3} = gck_used(Collect),
    #{gcks := [MC1, MC2, MC3]} = Global,
    Names1 = global_name(Used1, MC1, gck1, bufg_clk, Names0),
    Names2 = global_name(Used2, MC2, gck2, bufg_clk, Names1),
    Names3 = global_name(Used3, MC3, gck3, bufg_clk, Names2),
    Names3.

%%--------------------------------------------------------------------

gck_used(Collect) ->
    maps:fold(fun (Key, Value, Used) ->
        gck_used(Key, Value, Used)
    end, {false, false, false}, Collect).

%%--------------------------------------------------------------------

gck_used({_FB, cell, _MC}, #{bypass := yes}, Used) ->
    Used;
gck_used({_FB, cell, _MC}, #{clk_mux1 := yes, clk_mux0 := yes}, Used) ->
    Used;
gck_used({_FB, cell, _MC}, Cell = #{clk_mux1 := yes}, {_, Used2, Used3}) ->
    true = is_map_key(std_power, Cell),
    {true, Used2, Used3};
gck_used({_FB, cell, _MC}, Cell = #{clk_mux0 := yes}, {Used1, Used2, _}) ->
    true = is_map_key(std_power, Cell),
    {Used1, Used2, true};
gck_used({_FB, cell, _MC}, #{std_power := yes}, {Used1, _, Used3}) ->
    {Used1, true, Used3};
gck_used(_Key, _Value, Used) ->
    Used.

%%--------------------------------------------------------------------

gsr_name(Global, Collect, Names) ->
    Used = gsr_used(Collect),
    #{gsr := MC} = Global,
    global_name(Used, MC, gsr, bufg_sr, Names).

%%--------------------------------------------------------------------

gsr_used(Collect) ->
    maps:fold(fun (Key, Value, Used) ->
        gsr_used(Key, Value, Used)
    end, false, Collect).

%%--------------------------------------------------------------------

gsr_used({_FB, cell, _MC}, #{s_gsr := yes}, _Used) ->
    true;
gsr_used({_FB, cell, _MC}, #{r_gsr := yes}, _Used) ->
    true;
gsr_used(_Key, _Value, Used) ->
    Used.

%%--------------------------------------------------------------------

gts_names(Global, Collect, Names0) ->
    {Used1, Used2, Used3, Used4} = gts_used(Collect),
    case Global of
        #{gtss := [MC1, MC2]} ->
            Names1 = global_name(Used1, MC1, gts1, bufg_oe, Names0),
            Names2 = global_name(Used2, MC2, gts2, bufg_oe, Names1),
            Names2;

        #{gtss := [MC1, MC2, MC3, MC4]} ->
            Names1 = global_name(Used1, MC1, gts1, bufg_oe, Names0),
            Names2 = global_name(Used2, MC2, gts2, bufg_oe, Names1),
            Names3 = global_name(Used3, MC3, gts3, bufg_oe, Names2),
            Names4 = global_name(Used4, MC4, gts4, bufg_oe, Names3),
            Names4
    end.

%%--------------------------------------------------------------------

gts_used(Collect) ->
    maps:fold(fun (Key, Value, Used) ->
        gts_used(Key, Value, Used)
    end, {false, false, false, false}, Collect).

%%--------------------------------------------------------------------

gts_used({_FB, cell, _MC}, Cell, Used = {Used1, Used2, Used3, Used4}) ->
    case Cell of
        #{oe_gts := yes, oe_gts_mux1 := yes, oe_gts_mux0 := yes} ->
            {Used1, Used2, Used3, true};
        #{oe_gts := yes, oe_gts_mux1 := yes} ->
            {Used1, Used2, true, Used4};
        #{oe_gts := yes, oe_gts_mux0 := yes} ->
            {Used1, true, Used3, Used4};
        #{oe_gts := yes} ->
            {true, Used2, Used3, Used4};
        _ ->
            Used
    end;
gts_used(_Key, _Value, Used) ->
    Used.

%%--------------------------------------------------------------------

external_name({Pin, external}, {Global0, Dir}, PinNames) ->
    #{Pin := Name0} = PinNames,
    Global = atom_to_binary(Global0, latin1),
    Name = atom_to_binary(Name0, latin1),
    {<<Global/binary, "_", Name/binary>>, Dir};
external_name({Pin0, external}, Dir = inout, _PinNames) ->
    Pin = atom_to_binary(Pin0, latin1),
    {<<Pin/binary, "_in">>, Dir};
external_name({Pin0, external}, Dir, PinNames) ->
    #{Pin0 := Name0} = PinNames,
    Pin = atom_to_binary(Pin0, latin1),
    Name = atom_to_binary(Name0, latin1),
    {<<Pin/binary, "_", Name/binary>>, Dir};
external_name({_, internal}, Name, _PinNames) ->
    Name;
external_name({_, logic}, Name, _PinNames) ->
    Name;
external_name({Pin0, pin}, external, PinNames) ->
    #{Pin0 := Name0} = PinNames,
    Pin = atom_to_binary(Pin0, latin1),
    Name = atom_to_binary(Name0, latin1),
    <<Pin/binary, "_", Name/binary>>.

%%--------------------------------------------------------------------

internal_name({_, external}, Name, _Names) ->
    Name;
internal_name({Pin, internal}, internal, _Names) ->
    atom_to_binary(Pin, latin1);
internal_name({Pin, internal}, external, Names) ->
    #{{Pin, external} := {Name, _Dir}} = Names,
    Name;
internal_name({_, logic}, Name, _Names) ->
    Name;
internal_name({_, pin}, Name, _Names) ->
    Name.

%%--------------------------------------------------------------------

logic_name({_, external}, Name, _Names) ->
    Name;
logic_name({_, internal}, Name, _Names) ->
    Name;
logic_name({Pin, logic}, internal, _Names) ->
    atom_to_binary(Pin, latin1);
logic_name({Pin, logic}, external, Names) ->
    #{{Pin, external} := {Name, _Dir}} = Names,
    Name;
logic_name({_, pin}, Name, _Names) ->
    Name.

%%====================================================================
%% inputs
%%====================================================================

inputs(Density, Collect, Names) ->
    maps:fold(fun (Key, Value, Inputs) ->
        inputs(Density, Key, Value, Names, Inputs)
    end, #{}, Collect).

%%--------------------------------------------------------------------

inputs(Density, {FB, input, Input}, Muxes, Names, Inputs) ->
    Source = input_map:choice(Density, Input, choice(Muxes)),
    case Names of
        #{Source := {Name, _}} ->
            input(FB, Input, Name, Inputs);

        #{Source := Name} ->
            input(FB, Input, Name, Inputs)
    end;
inputs(_Density, _Key, _Value, _Names, Inputs) ->
    Inputs.

%%--------------------------------------------------------------------

input(FB, Input, Name, Inputs) ->
    case Inputs of
        #{FB := Names} ->
            Inputs#{FB => Names#{Input => Name}};

        _ ->
            Inputs#{FB => #{Input => Name}}
    end.

%%====================================================================
%% compile
%%====================================================================

compile(Collects = #{global := _Global}, Inputs, Names) ->
    Cells0 = maps:fold(fun (Key, Value, Cells) ->
        compile_type(Key, Value, Names, Cells)
    end, #{}, Collects),
    Cells1 = maps:fold(fun (Key, Value, Cells) ->
        compile_cell(Key, Value, Collects, Inputs, Names, Cells)
    end, Cells0, Collects),
    Cells1.

%%--------------------------------------------------------------------

compile_type({FB, cell, MC}, Collect, Names, Cells) ->
    case Collect of
        #{std_power := yes, bypass := yes} ->
            compile_type_add(FB, MC, bypass, Names, Cells);

        #{std_power := yes, t_type := yes} ->
            compile_type_add(FB, MC, t_type, Names, Cells);

        #{std_power := yes} ->
            compile_type_add(FB, MC, d_type, Names, Cells);

        _ ->
            false = is_map_key(t_type, Collect),
            Cells
    end;
compile_type(_Key, _Value, _Names, Cells) ->
    Cells.

%%--------------------------------------------------------------------

compile_type_add(FB, MC, Type, Names, Cells) ->
    Key = macro_cell:join(FB, MC),
    Base = atom_to_binary(Key, latin1),
    case Names of
        #{{Key, logic} := Name, {Key, pin} := Pin} ->
            Cell = #{
                base => Base,
                name => Name,
                type => Type,
                terms => [],
                pin => Pin
            },
            Cells#{Key => Cell};

        #{{Key, logic} := Name} ->
            Cell = #{
                base => Base,
                name => Name,
                type => Type,
                terms => []
            },
            Cells#{Key => Cell}
    end.

%%--------------------------------------------------------------------

compile_cell({FB, cell, MC}, Collect, Collects, Inputs0, Names, Cells0) ->
    #{global := Global} = Collects,
    Inputs = maps:get(FB, Inputs0, #{}),
    PT1 = compile_pt1(Collect, Inputs),
    PT2 = compile_pt2(Collect, Inputs),
    PT3 = compile_pt3(Collect, Inputs),
    PT4 = compile_pt4(Collect, Inputs),
    PT5 = compile_pt5(Collect, Inputs),
    Cells1 = compile_pt(FB, MC, Collect, PT1, Collects, Cells0),
    Cells2 = compile_pt(FB, MC, Collect, PT2, Collects, Cells1),
    Cells3 = compile_pt(FB, MC, Collect, PT3, Collects, Cells2),
    Cells4 = compile_pt(FB, MC, Collect, PT4, Collects, Cells3),
    Cells5 = compile_pt(FB, MC, Collect, PT5, Collects, Cells4),
    Cells6 = compile_invert(FB, MC, Collect, Cells5),
    Cells7 = compile_preset(FB, MC, Collect, Cells6),
    Cells8 = compile_clk(FB, MC, Collect, Global, Names, Cells7),
    Cells9 = compile_s(FB, MC, Collect, Global, Names, Cells8),
    Cells10 = compile_r(FB, MC, Collect, Global, Names, Cells9),
    Cells11 = compile_oe(FB, MC, Collect, Global, Names, Cells10),
    Cells11;
compile_cell(_Key, _Value, _Collects, _Inputs, _Names, Cells) ->
    Cells.

%%--------------------------------------------------------------------

compile_inputs([Input], Inputs) when is_atom(Input) ->
    #{Input := Name} = Inputs,
    Name;
compile_inputs([{invert, Input}], Inputs) ->
    #{Input := Name} = Inputs,
    [<<"NOT ">>, Name];
compile_inputs(Terms = [_ | _], Inputs) ->
    lists:join(<<" AND ">>, lists:sort(lists:map(fun (Term) ->
        compile_input(Term, Inputs)
    end, Terms))).

%%--------------------------------------------------------------------

compile_input(Input, Inputs) when is_atom(Input) ->
    #{Input := Name} = Inputs,
    Name;
compile_input({invert, Input}, Inputs) ->
    #{Input := Name} = Inputs,
    <<"(NOT ", Name/binary, ")">>.

%%--------------------------------------------------------------------

compile_pt(FB, MC, _Collect, {term, Term}, _Collects, Cells) ->
    compile_term(FB, MC, Term, Cells);
compile_pt(FB, MC, Collect, {forward, Term}, Collects, Cells) ->
    compile_forward(FB, MC, Collect, Term, Collects, Cells);
compile_pt(FB, MC, _Collect, {Port, Term}, _Collects, Cells) ->
    compile_port(FB, MC, Port, Term, Cells);
compile_pt(_FB, _MC, _Collect, false, _Collects, Cells) ->
    Cells.

%%--------------------------------------------------------------------

compile_pt1(Collect, Inputs) ->
    case Collect of
        #{pt1_mux1 := yes, pt1_mux0 := yes, pt1 := Terms, ce_or_s := yes} ->
            {ce, compile_inputs(Terms, Inputs)};

        #{pt1_mux1 := yes, pt1_mux0 := yes, pt1 := Terms} ->
            false = is_map_key(s_gsr, Collect),
            {s, compile_inputs(Terms, Inputs)};

        #{pt1_mux1 := yes, pt1 := Terms} ->
            {forward, compile_inputs(Terms, Inputs)};

        #{pt1_mux0 := yes, pt1 := Terms} ->
            {term, compile_inputs(Terms, Inputs)};

        _ ->
            false = is_map_key(pt1_std_power, Collect),
            false
    end.

%%--------------------------------------------------------------------

compile_pt2(Collect, Inputs) ->
    case Collect of
        #{pt2_mux1 := yes, pt2_mux0 := yes, pt2 := Terms} ->
            {term_xor, compile_inputs(Terms, Inputs)};

        #{pt2_mux1 := yes, pt2 := Terms} ->
            {forward, compile_inputs(Terms, Inputs)};

        #{pt2_mux0 := yes, pt2 := Terms} ->
            {term, compile_inputs(Terms, Inputs)};

        _ ->
            false = is_map_key(pt2_std_power, Collect),
            false
    end.

%%--------------------------------------------------------------------

compile_pt3(Collect, Inputs) ->
    case Collect of
        #{pt3_mux1 := yes, pt3_mux0 := yes, pt3 := Terms, clk_invert := yes} ->
            true = is_map_key(clk_mux0, Collect),
            true = is_map_key(clk_mux1, Collect),
            {clk, {invert, compile_inputs(Terms, Inputs)}};

        #{pt3_mux1 := yes, pt3_mux0 := yes, pt3 := Terms} ->
            true = is_map_key(clk_mux0, Collect),
            true = is_map_key(clk_mux1, Collect),
            {clk, compile_inputs(Terms, Inputs)};

        #{pt3_mux1 := yes, pt3 := Terms} ->
            {forward, compile_inputs(Terms, Inputs)};

        #{pt3_mux0 := yes, pt3 := Terms} ->
            {term, compile_inputs(Terms, Inputs)};

        _ ->
            false = is_map_key(pt3_std_power, Collect),
            false
    end.

%%--------------------------------------------------------------------

compile_pt4(Collect, Inputs) ->
    case Collect of
        #{pt4_mux1 := yes, pt4_mux0 := yes, pt4 := Terms, ce_or_r := yes} ->
            {ce, compile_inputs(Terms, Inputs)};

        #{pt4_mux1 := yes, pt4_mux0 := yes, pt4 := Terms} ->
            false = is_map_key(r_gsr, Collect),
            {r, compile_inputs(Terms, Inputs)};

        #{pt4_mux1 := yes, pt4 := Terms} ->
            {forward, compile_inputs(Terms, Inputs)};

        #{pt4_mux0 := yes, pt4 := Terms} ->
            {term, compile_inputs(Terms, Inputs)};

        _ ->
            false = is_map_key(pt4_std_power, Collect),
            false
    end.

%%--------------------------------------------------------------------

compile_pt5(Collect, Inputs) ->
    case Collect of
        #{pt5_mux1 := yes, pt5_mux0 := yes, pt5 := Terms, oe_invert := yes} ->
            false = is_map_key(oe_gts, Collect),
            {oe, {invert, compile_inputs(Terms, Inputs)}};

        #{pt5_mux1 := yes, pt5_mux0 := yes, pt5 := Terms} ->
            false = is_map_key(oe_gts, Collect),
            {oe, compile_inputs(Terms, Inputs)};

        #{pt5_mux1 := yes, pt5 := Terms} ->
            {forward, compile_inputs(Terms, Inputs)};

        #{pt5_mux0 := yes, pt5 := Terms} ->
            {term, compile_inputs(Terms, Inputs)};

        _ ->
            false = is_map_key(pt5_std_power, Collect),
            false
    end.

%%--------------------------------------------------------------------

compile_invert(FB, MC, #{invert := yes}, Cells) ->
    compile_port(FB, MC, term_invert, yes, Cells);
compile_invert(_FB, _MC, _Collect, Cells) ->
    Cells.

%%--------------------------------------------------------------------

compile_preset(FB, MC, #{preset := yes}, Cells) ->
    compile_port(FB, MC, preset, yes, Cells);
compile_preset(_FB, _MC, _Collect, Cells) ->
    Cells.

%%--------------------------------------------------------------------

compile_term(FB, MC, Term, Cells) ->
    Key = macro_cell:join(FB, MC),
    #{Key := Cell = #{terms := Terms}} = Cells,
    Cells#{Key => Cell#{terms => [Term | Terms]}}.

%%--------------------------------------------------------------------

compile_port(FB, MC, Port, Term, Cells) ->
    Key = macro_cell:join(FB, MC),
    #{Key := Cell} = Cells,
    Cells#{Key => Cell#{Port => Term}}.

%%--------------------------------------------------------------------

to_lower(mc01) -> mc02;
to_lower(mc02) -> mc03;
to_lower(mc03) -> mc04;
to_lower(mc04) -> mc05;
to_lower(mc05) -> mc06;
to_lower(mc06) -> mc07;
to_lower(mc07) -> mc08;
to_lower(mc08) -> mc09;
to_lower(mc09) -> mc10;
to_lower(mc10) -> mc11;
to_lower(mc11) -> mc12;
to_lower(mc12) -> mc13;
to_lower(mc13) -> mc14;
to_lower(mc14) -> mc15;
to_lower(mc15) -> mc16;
to_lower(mc16) -> mc17;
to_lower(mc17) -> mc18;
to_lower(mc18) -> mc01.

%%--------------------------------------------------------------------

to_upper(mc01) -> mc18;
to_upper(mc02) -> mc01;
to_upper(mc03) -> mc02;
to_upper(mc04) -> mc03;
to_upper(mc05) -> mc04;
to_upper(mc06) -> mc05;
to_upper(mc07) -> mc06;
to_upper(mc08) -> mc07;
to_upper(mc09) -> mc08;
to_upper(mc10) -> mc09;
to_upper(mc11) -> mc10;
to_upper(mc12) -> mc11;
to_upper(mc13) -> mc12;
to_upper(mc14) -> mc13;
to_upper(mc15) -> mc14;
to_upper(mc16) -> mc15;
to_upper(mc17) -> mc16;
to_upper(mc18) -> mc17.

%%--------------------------------------------------------------------

compile_forward(FB, MC0, Collect0, Term, Collects, Cells) ->
    case Collect0 of
        #{to_upper := yes} ->
            MC = to_upper(MC0),
            #{{FB, cell, MC} := Collect} = Collects,
            compile_from_lower(FB, MC, Collect, Term, Collects, Cells);

        _ ->
            MC = to_lower(MC0),
            #{{FB, cell, MC} := Collect} = Collects,
            compile_from_upper(FB, MC, Collect, Term, Collects, Cells)
    end.

%%--------------------------------------------------------------------

compile_from_lower(FB, MC, Collect, Term, Collects, Cells) ->
    case Collect of
        #{from_lower := yes} ->
            compile_term(FB, MC, Term, Cells);

        _ ->
            compile_forward(FB, MC, Collect, Term, Collects, Cells)
    end.

%%--------------------------------------------------------------------

compile_from_upper(FB, MC, Collect, Term, Collects, Cells) ->
    case Collect of
        #{from_upper := yes} ->
            compile_term(FB, MC, Term, Cells);

        _ ->
            compile_forward(FB, MC, Collect, Term, Collects, Cells)
    end.

%%--------------------------------------------------------------------

compile_clk(FB, MC, Collect, Global, Names, Cells) ->
    case Collect of
        #{bypass := yes} ->
            Cells;

        #{clk_mux1 := yes, clk_mux0 := yes} ->
            Cells;

        #{clk_mux1 := yes} ->
            true = is_map_key(std_power, Collect),
            compile_gck(FB, MC, Collect, 1, gck1_enable, Global, Names, Cells);

        #{clk_mux0 := yes} ->
            true = is_map_key(std_power, Collect),
            compile_gck(FB, MC, Collect, 3, gck3_enable, Global, Names, Cells);

        #{std_power := yes} ->
            compile_gck(FB, MC, Collect, 2, gck2_enable, Global, Names, Cells);

        _ ->
            Cells
    end.

%%--------------------------------------------------------------------

compile_gck(FB, MC, #{clk_invert := yes}, N, Enable, Global, Names, Cells) ->
    #{gcks := GCKs, Enable := yes} = Global,
    GCK = lists:nth(N, GCKs),
    #{{GCK, external} := {Name, _}} = Names,
    compile_port(FB, MC, clk, {invert, Name}, Cells);
compile_gck(FB, MC, #{}, N, Enable, Global, Names, Cells) ->
    #{gcks := GCKs, Enable := yes} = Global,
    GCK = lists:nth(N, GCKs),
    #{{GCK, external} := {Name, _}} = Names,
    compile_port(FB, MC, clk, Name, Cells).

%%--------------------------------------------------------------------

compile_s(FB, MC, Collect, Global, Names, Cells) ->
    case Collect of
        #{bypass := yes} ->
            Cells;

        #{s_gsr := yes} ->
            compile_gsr(FB, MC, s, Global, Names, Cells);

        _ ->
            Cells
    end.

%%--------------------------------------------------------------------

compile_r(FB, MC, Collect, Global, Names, Cells) ->
    case Collect of
        #{bypass := yes} ->
            Cells;

        #{r_gsr := yes} ->
            compile_gsr(FB, MC, r, Global, Names, Cells);

        _ ->
            Cells
    end.

%%--------------------------------------------------------------------

compile_gsr(FB, MC, Port, #{gsr := GSR, gsr_invert := yes}, Names, Cells) ->
    #{{GSR, external} := {Name, _}} = Names,
    compile_port(FB, MC, Port, {invert, Name}, Cells);
compile_gsr(FB, MC, Port, #{gsr := GSR}, Names, Cells) ->
    #{{GSR, external} := {Name, _}} = Names,
    compile_port(FB, MC, Port, Name, Cells).

%%--------------------------------------------------------------------

compile_oe(FB, MC, Collect, Global, Names, Cells) ->
    case Collect of
        #{oe_gts := yes, oe_gts_mux1 := yes, oe_gts_mux0 := yes} ->
            compile_gts(FB, MC, Collect, 4, gts4_enable, Global, Names, Cells);

        #{oe_gts := yes, oe_gts_mux1 := yes} ->
            compile_gts(FB, MC, Collect, 3, gts3_enable, Global, Names, Cells);

        #{oe_gts := yes, oe_gts_mux0 := yes} ->
            compile_gts(FB, MC, Collect, 2, gts2_enable, Global, Names, Cells);

        #{oe_gts := yes} ->
            compile_gts(FB, MC, Collect, 1, gts1_enable, Global, Names, Cells);

        _ ->
            Cells
    end.

%%--------------------------------------------------------------------

compile_gts(FB, MC, #{oe_invert := yes}, N, Enable, Global, Names, Cells) ->
    #{gtss := GTSs, Enable := yes} = Global,
    GTS = lists:nth(N, GTSs),
    #{{GTS, external} := {Name, _}} = Names,
    compile_port(FB, MC, oe, {invert, Name}, Cells);
compile_gts(FB, MC, #{}, N, Enable, Global, Names, Cells) ->
    #{gtss := GTSs, Enable := yes} = Global,
    GTS = lists:nth(N, GTSs),
    #{{GTS, external} := {Name, _}} = Names,
    compile_port(FB, MC, oe, Name, Cells).

%%====================================================================
%% UCF
%%====================================================================

ucf(Constraints) ->
    lists:map(fun (Name) ->
        ucf(Name, Constraints)
    end, lists:sort(maps:to_list(Constraints))).

%%--------------------------------------------------------------------

ucf({{MC, external}, {Net, bufg_clk}}, _Names) ->
    LOC = macro_cell:name(MC),
    <<"NET \"", Net/binary, "\" LOC = \"", LOC/binary, "\" | BUFG = CLK;\n">>;
ucf({{MC, external}, {Net, bufg_sr}}, _Names) ->
    LOC = macro_cell:name(MC),
    <<"NET \"", Net/binary, "\" LOC = \"", LOC/binary, "\" | BUFG = SR;\n">>;
ucf({{MC, external}, {Net, bufg_oe}}, _Names) ->
    LOC = macro_cell:name(MC),
    <<"NET \"", Net/binary, "\" LOC = \"", LOC/binary, "\" | BUFG = OE;\n">>;
ucf({{MC, external}, {_, inout}}, Names) ->
    #{{MC, pin} := Net} = Names,
    LOC = macro_cell:name(MC),
    <<"NET \"", Net/binary, "\" LOC = \"", LOC/binary, "\";\n">>;
ucf({{MC, external}, {Net, _}}, _Names) ->
    LOC = macro_cell:name(MC),
    <<"NET \"", Net/binary, "\" LOC = \"", LOC/binary, "\";\n">>;
ucf({{_, internal}, _}, _Names) ->
    <<>>;
ucf({{_, logic}, _}, _Names) ->
    <<>>;
ucf({{_, pin}, _}, _Names) ->
    <<>>.


%%====================================================================
%% VHDL
%%====================================================================

vhdl(Cells, Names) ->
    [   <<
        "library IEEE;\n"
        "use IEEE.STD_LOGIC_1164.ALL;\n"
        "library UNISIM;\n"
        "use UNISIM.vcomponents.ALL;\n"
        "\n"
        "entity experiment is\n"
        "  port (\n"
        >>,
        vhdl_ports(Names),
        <<
        "  );\n"
        "end experiment;\n"
        "\n"
        "architecture behavioral of experiment is\n"
        >>,
        lists:map(fun vhdl_signal/1, lists:sort(maps:to_list(Cells))),
        <<"begin\n">>,
        lists:map(fun vhdl_cell/1, lists:sort(maps:to_list(Cells))),
        <<"end behavioral;\n">>
    ].

%%--------------------------------------------------------------------

vhdl_ports(Names) ->
    Ports = lists:filtermap(fun (Name) ->
        vhdl_port(Name, Names)
    end, lists:sort(maps:to_list(Names))),
    [lists:join(<<";\n">>, Ports), <<"\n">>].

%%--------------------------------------------------------------------

vhdl_port({{_, external}, {Name, Dir}}, _Names)
        when Dir =:= in;
             Dir =:= bufg_clk;
             Dir =:= bufg_sr;
             Dir =:= bufg_oe ->
    {true, [<<"    ">>, Name, <<" : in STD_LOGIC">>]};
vhdl_port({{MC, external}, {_, inout}}, Names) ->
    #{{MC, pin} := Name} = Names,
    {true, [<<"    ">>, Name, <<" : inout STD_LOGIC">>]};
vhdl_port({{_, external}, {Name, out}}, _Names) ->
    {true, [<<"    ">>, Name, <<" : out STD_LOGIC">>]};
vhdl_port({{_, internal}, _}, _Names) ->
    false;
vhdl_port({{_, logic}, _}, _Names) ->
    false;
vhdl_port({{_, pin}, _}, _Names) ->
    false.

%%--------------------------------------------------------------------

vhdl_signal({_MC, Cell = #{base := Base, oe := OE, pin := _}}) ->
    SignalPin = vhdl_signal_for([Base, <<"_in">>]),
    SignalE = vhdl_signal_term([Base, <<"_oe">>], OE),
    Logic = vhdl_signal_logic(Base, Cell),
    [
        SignalPin,
        SignalE,
        Logic
    ];
vhdl_signal({_MC, Cell = #{base := Base, oe := OE}}) ->
    SignalE = vhdl_signal_term([Base, <<"_oe">>], OE),
    Logic = vhdl_signal_logic(Base, Cell),
    [
        SignalE,
        Logic
    ];
vhdl_signal({_MC, Cell = #{base := Base, pin := _}}) ->
    Signal = vhdl_signal_for(Base),
    Logic = vhdl_signal_logic(Base, Cell),
    [
        Signal,
        Logic
    ];
vhdl_signal({_MC, Cell = #{name := Name}}) ->
    vhdl_signal_logic(Name, Cell).

%%--------------------------------------------------------------------

vhdl_signal_logic(_Name, Cell = #{type := bypass}) ->
    vhdl_signal_internal(Cell);
vhdl_signal_logic(_Name, Cell = #{base := Base, type := d_type}) ->
    SignalQ = vhdl_signal_internal(Cell),
    SignalD = vhdl_signal_terms([Base, <<"_d">>], Cell),
    SignalCLR = vhdl_signal_port(Cell, s, <<"_clr">>),
    SignalPRE = vhdl_signal_port(Cell, s, <<"_pre">>),
    SignalCE = vhdl_signal_port(Cell, ce, <<"_ce">>),
    [
        SignalQ,
        SignalD,
        SignalCLR,
        SignalPRE,
        SignalCE
    ];
vhdl_signal_logic(_Name, Cell = #{base := Base, type := t_type}) ->
    SignalQ = vhdl_signal_internal(Cell),
    SignalT = vhdl_signal_terms([Base, <<"_t">>], Cell),
    SignalCLR = vhdl_signal_port(Cell, s, <<"_clr">>),
    SignalPRE = vhdl_signal_port(Cell, s, <<"_pre">>),
    SignalCE = vhdl_signal_port(Cell, ce, <<"_ce">>),
    [
        SignalQ,
        SignalT,
        SignalCLR,
        SignalPRE,
        SignalCE
    ].

%%--------------------------------------------------------------------

vhdl_signal_internal(#{base := Base, name := Base}) ->
    vhdl_signal_for(Base);
vhdl_signal_internal(#{base := Base, oe := _}) ->
    vhdl_signal_for(Base);
vhdl_signal_internal(_Cell) ->
    <<>>.

%%--------------------------------------------------------------------

vhdl_signal_term(_Name, Term) when is_binary(Term) ->
    <<>>;
vhdl_signal_term(Name, _Term) ->
    vhdl_signal_for(Name).

%%--------------------------------------------------------------------

vhdl_signal_terms(_Name, #{terms := [], term_invert := yes}) ->
    <<>>;
vhdl_signal_terms(_Name, #{terms := []}) ->
    <<>>;
vhdl_signal_terms(Name, #{terms := [_], term_xor := _}) ->
    vhdl_signal_for(Name);
vhdl_signal_terms(Name, #{terms := Terms, term_xor := Xor, term_invert := yes}) ->
    throw({Name, terms, Terms, x_or, Xor, invert});
vhdl_signal_terms(Name, #{terms := Terms, term_xor := Xor}) ->
    throw({Name, terms, Terms, x_or, Xor});
vhdl_signal_terms(Name, #{terms := [_], term_invert := yes}) ->
    vhdl_signal_for(Name);
vhdl_signal_terms(_Name, #{terms := [Term]}) when is_binary(Term) ->
    <<>>;
vhdl_signal_terms(Name, #{terms := [_]}) ->
    vhdl_signal_for(Name);
vhdl_signal_terms(Name, #{terms := _, term_invert := yes}) ->
    vhdl_signal_for(Name);
vhdl_signal_terms(Name, _Cell) ->
    vhdl_signal_for(Name).

%%--------------------------------------------------------------------

vhdl_signal_port(Cell, Key, Under) ->
    case Cell of
        #{Key := Term, base := Base} ->
            vhdl_signal_term([Base, Under], Term);

        _ ->
            <<>>
    end.

%%--------------------------------------------------------------------

vhdl_signal_for(Name) ->
    [<<"  signal ">>, Name, <<" : STD_LOGIC;\n">>].

%%--------------------------------------------------------------------

vhdl_cell({_MC, Cell = #{base := Base, oe := OE, pin := Pin}}) ->
    Term = vhdl_logic(Base, Cell),
    {LineE, NameE} = vhdl_term([Base, <<"_oe">>], OE),
    [
        <<"  ">>, Base, <<"_ibuf : IBUF port map (\n">>,
        <<"    I => ">>, Pin, <<",\n">>,
        <<"    O => ">>, Base, <<"_in\n">>,
        <<"  );\n">>,
        Term,
        LineE,
        <<"  ">>, Base, <<"_obuf : OBUFE port map (\n">>,
        <<"    I => ">>, Base, <<",\n">>,
        <<"    O => ">>, Pin, <<",\n">>,
        <<"    E => ">>, NameE, <<"\n">>,
        <<"  );\n">>
    ];
vhdl_cell({_MC, Cell = #{base := Base, name := Name, oe := OE}}) ->
    Term = vhdl_logic(Base, Cell),
    {LineE, NameE} = vhdl_term([Base, <<"_oe">>], OE),
    [
        Term,
        LineE,
        <<"  ">>, Base, <<"_obuf : OBUFE port map (\n">>,
        <<"    I => ">>, Base, <<",\n">>,
        <<"    O => ">>, Name, <<",\n">>,
        <<"    E => ">>, NameE, <<"\n">>,
        <<"  );\n">>
    ];
vhdl_cell({_MC, Cell = #{base := Base, name := Name, pin := _}}) ->
    Term = vhdl_logic(Base, Cell),
    [
        Term,
        <<"  ">>, Name, <<" <= ">>, Base, <<";\n">>
    ];
vhdl_cell({_MC, Cell = #{name := Name}}) ->
    vhdl_logic(Name, Cell).

%%--------------------------------------------------------------------

vhdl_logic(Name, Cell = #{type := bypass}) ->
    case vhdl_terms(Name, Cell) of
        {<<>>, Value} ->
            [<<"  ">>, Name, <<" <= ">>, Value, <<";\n">>];

        {Line, _Name} ->
            Line
    end;
vhdl_logic(Name, Cell = #{base := Base, type := d_type}) ->
    {LineD, NameD} = vhdl_terms([Base, <<"_d">>], Cell),
    {LineCLR, NameCLR, TypeCLR} =
        vhdl_ff_port(Cell, s, <<"_clr">>, <<"CLR">>, <<"C">>),
    {LinePRE, NamePRE, TypePRE} =
        vhdl_ff_port(Cell, s, <<"_pre">>, <<"PRE">>, <<"P">>),
    {LineCE, NameCE, TypeCE} =
        vhdl_ff_port(Cell, ce, <<"_ce">>, <<"CE">>, <<"E">>),
    {NameCLR0, TypeCLR0} = vhdl_special_d_type(TypeCLR, TypePRE, TypeCE),
    Type = [<<"FD">>, TypeCLR0, TypeCLR, TypePRE, TypeCE],
    vhdl_ff(Name, Cell, Type, [
        LineD,
        LineCLR,
        LinePRE,
        LineCE
    ], [
        <<"    D => ">>, NameD, <<",\n">>,
        NameCLR0,
        NameCLR,
        NamePRE,
        NameCE
    ]);
vhdl_logic(Name, Cell = #{base := Base, type := t_type}) ->
    {LineT, NameT} = vhdl_terms([Base, <<"_t">>], Cell),
    {LineCLR, NameCLR, TypeCLR} =
        vhdl_ff_port(Cell, s, <<"_clr">>, <<"CLR">>, <<"C">>),
    {LinePRE, NamePRE, TypePRE} =
        vhdl_ff_port(Cell, s, <<"_pre">>, <<"PRE">>, <<"P">>),
    {LineCE, NameCE, TypeCE} =
        vhdl_ff_port(Cell, ce, <<"_ce">>, <<"CE">>, <<"E">>),
    {NameCLR0, TypeCLR0} = vhdl_special_t_type(TypeCLR, TypePRE, TypeCE),
    Type = [<<"FT">>, TypeCLR0, TypeCLR, TypePRE, TypeCE],
    vhdl_ff(Name, Cell, Type, [
        LineT,
        LineCLR,
        LinePRE,
        LineCE
    ], [
        <<"    T => ">>, NameT, <<",\n">>,
        NameCLR0,
        NameCLR,
        NamePRE,
        NameCE
    ]).

%%--------------------------------------------------------------------

vhdl_ff(Name, Cell, Type, Lines, Names) ->
    #{base := Base, clk := Clk} = Cell,
    Init = case Cell of
        #{preset := yes} ->
            <<" generic map (INIT => '1')">>;

        _ ->
            <<>>
    end,
    {LineC, NameC} = vhdl_term([Base, <<"_c">>], Clk),
    [
        LineC,
        Lines,
        <<"  ">>, Base, <<"_ff: ">>, Type, Init, <<" port map (\n">>,
        <<"    C => ">>, NameC, <<",\n">>,
        Names,
        <<"    Q => ">>, Name, <<"\n">>,
        <<"  );\n">>
    ].

%%--------------------------------------------------------------------

vhdl_ff_port(Cell, Key, Under, Port, Letter) ->
    case Cell of
        #{Key := Term, base := Base} ->
            {Line, Name0} = vhdl_term([Base, Under], Term),
            Name = [<<"    ">>, Port, <<" => ">>, Name0, <<",\n">>],
            {Line, Name, Letter};

        _ ->
            {<<>>, <<>>, <<>>}
    end.

%%--------------------------------------------------------------------

vhdl_special_d_type(<<>>, <<>>, <<"E">>) ->
    Name = [<<"    CLR => '0',\n">>],
    {Name, <<"C">>};
vhdl_special_d_type(_CLR, _PRE, _CE) ->
    {<<>>, <<>>}.

%%--------------------------------------------------------------------

vhdl_special_t_type(<<>>, <<>>, <<>>) ->
    Name = [<<"    CLR => '0',\n">>],
    {Name, <<"C">>};
vhdl_special_t_type(_CLR, _PRE, _CE = <<>>) ->
    {<<>>, <<>>}.

%%--------------------------------------------------------------------

vhdl_terms(_Name, #{terms := [], term_invert := yes}) ->
    {<<>>, <<"'1'">>};
vhdl_terms(_Name, #{terms := []}) ->
    {<<>>, <<"'0'">>};
vhdl_terms(Name, #{terms := [Term0], term_xor := Xor0}) ->
    Term = vhdl_term_bracket(Term0),
    Xor = vhdl_term_bracket(Xor0),
    {[<<"  ">>, Name, <<" <= ">>, Term, <<" XOR ">>, Xor, <<";\n">>], Name};
vhdl_terms(Name, #{terms := Terms, term_xor := Xor, term_invert := yes}) ->
    throw({Name, terms, Terms, x_or, Xor, invert});
vhdl_terms(Name, #{terms := Terms, term_xor := Xor}) ->
    throw({Name, terms, Terms, x_or, Xor});
vhdl_terms(Name, #{terms := [Term0], term_invert := yes}) ->
    Term = vhdl_term_bracket(Term0),
    {[<<"  ">>, Name, <<" <= NOT ">>, Term, <<";\n">>], Name};
vhdl_terms(_Name, #{terms := [Term]}) when is_binary(Term) ->
    {<<>>, Term};
vhdl_terms(Name, #{terms := [Term]}) ->
    {[<<"  ">>, Name, <<" <= ">>, Term, <<";\n">>], Name};
vhdl_terms(Name, #{terms := Terms, term_invert := yes}) ->
    Head = [<<"  ">>, Name, <<" <= NOT (\n">>],
    Lines = lists:map(fun (Term) ->
        [<<"    ">>, vhdl_term_bracket(Term)]
    end, lists:sort(Terms)),
    Tail = <<"\n  );\n">>,
    {[Head, lists:join(<<" OR\n">>, Lines), Tail], Name};
vhdl_terms(Name, #{terms := Terms}) ->
    Head = [<<"  ">>, Name, <<" <= (\n">>],
    Lines = lists:map(fun (Term) ->
        [<<"    ">>, vhdl_term_bracket(Term)]
    end, lists:sort(Terms)),
    Tail = <<"\n  );\n">>,
    {[Head, lists:join(<<" OR\n">>, Lines), Tail], Name}.

%%--------------------------------------------------------------------

vhdl_term_bracket(Term) when is_binary(Term) ->
    Term;
vhdl_term_bracket(Term) ->
    [<<"(">>, Term, <<")">>].

%%--------------------------------------------------------------------

vhdl_term(_Name, Term) when is_binary(Term) ->
    {<<>>, Term};
vhdl_term(Name, Term) ->
    {[<<"  ">>, Name, <<" <= ">>, Term, <<";\n">>], Name}.

