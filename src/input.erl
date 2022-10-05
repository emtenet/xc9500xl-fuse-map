-module(input).

-export([assert/1]).
-export([from/1]).
-export([list/0]).
-export([number/1]).

-type input() ::
    input01 |
    input02 |
    input03 |
    input04 |
    input05 |
    input06 |
    input07 |
    input08 |
    input09 |
    input10 |
    input11 |
    input12 |
    input13 |
    input14 |
    input15 |
    input16 |
    input17 |
    input18 |
    input19 |
    input20 |
    input21 |
    input22 |
    input23 |
    input24 |
    input25 |
    input26 |
    input27 |
    input28 |
    input29 |
    input30 |
    input31 |
    input32 |
    input33 |
    input34 |
    input35 |
    input36 |
    input37 |
    input38 |
    input39 |
    input40 |
    input41 |
    input42 |
    input43 |
    input44 |
    input45 |
    input46 |
    input47 |
    input48 |
    input49 |
    input50 |
    input51 |
    input52 |
    input53 |
    input54.
-export_type([input/0]).

-type choice() :: non_neg_integer().
-export_type([choice/0]).

%%====================================================================
%% assert
%%====================================================================

assert(input01) -> ok;
assert(input02) -> ok;
assert(input03) -> ok;
assert(input04) -> ok;
assert(input05) -> ok;
assert(input06) -> ok;
assert(input07) -> ok;
assert(input08) -> ok;
assert(input09) -> ok;
assert(input10) -> ok;
assert(input11) -> ok;
assert(input12) -> ok;
assert(input13) -> ok;
assert(input14) -> ok;
assert(input15) -> ok;
assert(input16) -> ok;
assert(input17) -> ok;
assert(input18) -> ok;
assert(input19) -> ok;
assert(input20) -> ok;
assert(input21) -> ok;
assert(input22) -> ok;
assert(input23) -> ok;
assert(input24) -> ok;
assert(input25) -> ok;
assert(input26) -> ok;
assert(input27) -> ok;
assert(input28) -> ok;
assert(input29) -> ok;
assert(input30) -> ok;
assert(input31) -> ok;
assert(input32) -> ok;
assert(input33) -> ok;
assert(input34) -> ok;
assert(input35) -> ok;
assert(input36) -> ok;
assert(input37) -> ok;
assert(input38) -> ok;
assert(input39) -> ok;
assert(input40) -> ok;
assert(input41) -> ok;
assert(input42) -> ok;
assert(input43) -> ok;
assert(input44) -> ok;
assert(input45) -> ok;
assert(input46) -> ok;
assert(input47) -> ok;
assert(input48) -> ok;
assert(input49) -> ok;
assert(input50) -> ok;
assert(input51) -> ok;
assert(input52) -> ok;
assert(input53) -> ok;
assert(input54) -> ok.

%%====================================================================
%% from
%%====================================================================

from( 1) -> input01;
from( 2) -> input02;
from( 3) -> input03;
from( 4) -> input04;
from( 5) -> input05;
from( 6) -> input06;
from( 7) -> input07;
from( 8) -> input08;
from( 9) -> input09;
from(10) -> input10;
from(11) -> input11;
from(12) -> input12;
from(13) -> input13;
from(14) -> input14;
from(15) -> input15;
from(16) -> input16;
from(17) -> input17;
from(18) -> input18;
from(19) -> input19;
from(20) -> input20;
from(21) -> input21;
from(22) -> input22;
from(23) -> input23;
from(24) -> input24;
from(25) -> input25;
from(26) -> input26;
from(27) -> input27;
from(28) -> input28;
from(29) -> input29;
from(30) -> input30;
from(31) -> input31;
from(32) -> input32;
from(33) -> input33;
from(34) -> input34;
from(35) -> input35;
from(36) -> input36;
from(37) -> input37;
from(38) -> input38;
from(39) -> input39;
from(40) -> input40;
from(41) -> input41;
from(42) -> input42;
from(43) -> input43;
from(44) -> input44;
from(45) -> input45;
from(46) -> input46;
from(47) -> input47;
from(48) -> input48;
from(49) -> input49;
from(50) -> input50;
from(51) -> input51;
from(52) -> input52;
from(53) -> input53;
from(54) -> input54.

%%====================================================================
%% list
%%====================================================================

list() ->
    [
        input01,
        input02,
        input03,
        input04,
        input05,
        input06,
        input07,
        input08,
        input09,
        input10,
        input11,
        input12,
        input13,
        input14,
        input15,
        input16,
        input17,
        input18,
        input19,
        input20,
        input21,
        input22,
        input23,
        input24,
        input25,
        input26,
        input27,
        input28,
        input29,
        input30,
        input31,
        input32,
        input33,
        input34,
        input35,
        input36,
        input37,
        input38,
        input39,
        input40,
        input41,
        input42,
        input43,
        input44,
        input45,
        input46,
        input47,
        input48,
        input49,
        input50,
        input51,
        input52,
        input53,
        input54
    ].

%%====================================================================
%% number
%%====================================================================

number(input01) ->  1;
number(input02) ->  2;
number(input03) ->  3;
number(input04) ->  4;
number(input05) ->  5;
number(input06) ->  6;
number(input07) ->  7;
number(input08) ->  8;
number(input09) ->  9;
number(input10) -> 10;
number(input11) -> 11;
number(input12) -> 12;
number(input13) -> 13;
number(input14) -> 14;
number(input15) -> 15;
number(input16) -> 16;
number(input17) -> 17;
number(input18) -> 18;
number(input19) -> 19;
number(input20) -> 20;
number(input21) -> 21;
number(input22) -> 22;
number(input23) -> 23;
number(input24) -> 24;
number(input25) -> 25;
number(input26) -> 26;
number(input27) -> 27;
number(input28) -> 28;
number(input29) -> 29;
number(input30) -> 30;
number(input31) -> 31;
number(input32) -> 32;
number(input33) -> 33;
number(input34) -> 34;
number(input35) -> 35;
number(input36) -> 36;
number(input37) -> 37;
number(input38) -> 38;
number(input39) -> 39;
number(input40) -> 40;
number(input41) -> 41;
number(input42) -> 42;
number(input43) -> 43;
number(input44) -> 44;
number(input45) -> 45;
number(input46) -> 46;
number(input47) -> 47;
number(input48) -> 48;
number(input49) -> 49;
number(input50) -> 50;
number(input51) -> 51;
number(input52) -> 52;
number(input53) -> 53;
number(input54) -> 54.

