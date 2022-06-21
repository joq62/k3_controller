%%%-------------------------------------------------------------------
%% @doc k3_controller public API
%% @end
%%%-------------------------------------------------------------------

-module(k3_controller_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    k3_controller_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
