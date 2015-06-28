-module(listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Socket} = gen_tcp:listen(5000, [{reuseaddr, true}]),
    {ok, _Ref} = prim_inet:async_accept(Socket, -1),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSock, _Ref, {ok, Client}}, State) ->
    {ok, PID} = client:start_link(),
    inet_db:register_socket(Client, inet_tcp),
    gen_tcp:send(Client, <<"Welcome\r\n">>),
    gen_tcp:controlling_process(Client, PID),
    gen_server:call(PID, {assign, Client}),
    {ok, _Ref2} = prim_inet:async_accept(LSock, -1),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

