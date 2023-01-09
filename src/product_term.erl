-module(product_term).

-export([assert/1]).
-export([from/1]).

-export_type([product_term/0]).

-type product_term() ::
    pt1 |
    pt2 |
    pt3 |
    pt4 |
    pt5.

%%====================================================================
%% assert
%%====================================================================

-spec assert(product_term()) -> ok.

assert(pt1) -> ok;
assert(pt2) -> ok;
assert(pt3) -> ok;
assert(pt4) -> ok;
assert(pt5) -> ok.

%%====================================================================
%% from
%%====================================================================

-spec from(1..5) -> product_term().

from(1) -> pt1;
from(2) -> pt2;
from(3) -> pt3;
from(4) -> pt4;
from(5) -> pt5.

