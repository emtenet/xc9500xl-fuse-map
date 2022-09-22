-module(user_default).

-export([run/1]).

%%====================================================================
%% run
%%====================================================================

run(Module) ->
    case c:c(Module) of
        {ok, _} ->
            Module:run();

        Error ->
            Error
    end.

