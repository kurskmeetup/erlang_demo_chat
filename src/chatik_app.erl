-module(chatik_app).

-behaviour(application).

%% Application callbacks
-export([start/0, stop/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(chatik).

stop() ->
    application:stop(chatik).

start(_StartType, _StartArgs) ->
    chatik_sup:start_link().

stop(_State) ->
    ok.
