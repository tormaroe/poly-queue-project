%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc pqd.

-module(pqd).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the pqd server.
start() ->
    pqd_deps:ensure(),
    ensure_started(crypto),
    application:start(pqd).


%% @spec stop() -> ok
%% @doc Stop the pqd server.
stop() ->
    application:stop(pqd).
