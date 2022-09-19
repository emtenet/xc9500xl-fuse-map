-module(package).

-export([list/0]).
-export([pin_count/1]).

%%====================================================================
%% list
%%====================================================================

list() ->
    [
        bg256,
        cs48,
        cs144,
        cs280,
        fg256,
        tq100,
        tq144,
        vq44,
        vq64
    ].

%%====================================================================
%% pin_count
%%====================================================================

pin_count(bg256) -> 256;
pin_count(cs48) -> 48;
pin_count(cs144) -> 144;
pin_count(cs280) -> 280;
pin_count(fg256) -> 256;
pin_count(tq100) -> 100;
pin_count(tq144) -> 144;
pin_count(vq44) -> 44;
pin_count(vq64) -> 64;
pin_count(Device) ->
    pin_count(device:package(Device)).

