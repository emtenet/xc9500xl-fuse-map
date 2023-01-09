-module(input_map).

-export([choices/1]).
-export([choice/3]).
-export([sources/3]).

-type density() :: density:density().
-type input() :: input:input().
-type choice() :: non_neg_integer().

-type macro_cell() :: macro_cell:absolute().
-type realm() :: input:realm().
-type source() :: input:source().

%%====================================================================
%% choices
%%====================================================================

-spec choices(density())
    -> [choice()].

choices(xc9536xl) -> xc9536xl_input_map:choices();
choices(xc9572xl) -> xc9572xl_input_map:choices();
choices(xc95144xl) -> xc95144xl_input_map:choices();
choices(xc95288xl) -> xc95288xl_input_map:choices().

%%====================================================================
%%====================================================================
%% choice
%%====================================================================

-spec choice(density(), input(), choice())
    -> source() | unknown.

choice(xc9536xl, Input, Choice) -> xc9536xl_input_map:choice(Input, Choice);
choice(xc9572xl, Input, Choice) -> xc9572xl_input_map:choice(Input, Choice);
choice(xc95144xl, Input, Choice) -> xc95144xl_input_map:choice(Input, Choice);
choice(xc95288xl, Input, Choice) -> xc95288xl_input_map:choice(Input, Choice).

%%====================================================================
%% sources
%%====================================================================

-spec sources(density(), macro_cell(), realm())
    -> {choice(), [input()]} | no_pin | unknown.

sources(xc9536xl, MC, Dir) -> xc9536xl_input_map:sources(MC, Dir);
sources(xc9572xl, MC, Dir) -> xc9572xl_input_map:sources(MC, Dir);
sources(xc95144xl, MC, Dir) -> xc95144xl_input_map:sources(MC, Dir);
sources(xc95288xl, MC, Dir) -> xc95288xl_input_map:sources(MC, Dir).

