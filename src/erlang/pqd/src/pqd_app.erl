%% @author Mochi Media <dev@mochimedia.com>
%% @copyright pqd Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the pqd application.

-module(pqd_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for pqd.
start(_Type, _StartArgs) ->
    pqd_deps:ensure(),
    pqd_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for pqd.
stop(_State) ->
    ok.
