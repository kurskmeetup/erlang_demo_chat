-module(client).
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
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #{
        }}.

handle_call({assign, Socket}, _From, State) ->
    inet:setopts(Socket, [{active, true}, {packet, line}, binary]),
    gen_tcp:send(Socket, <<"Your nick: ">>),
    {reply, ok, maps:put(socket,Socket,State)};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({msg, From, Msg}, State) ->
    Socket=maps:get(socket,State),
    gen_tcp:send(Socket, <<From/binary,": ",Msg/binary,"\r\n">>),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,_Port,Payload0}, State) ->
    Socket=maps:get(socket,State),
    Payload=hd(binary:split(Payload0,<<"\r">>)),
    case maps:get(nick,State,undefined) of
        undefined -> 
            gen_tcp:send(Socket, <<"Hello, ",Payload/binary,"\r\n">>),
            gen_server:cast(room, {join, Payload, self()}),
            {noreply, maps:put(nick, Payload, State)};
        Nick ->
            case Payload of 
                <<>> ->
                    ok;
                _ ->
                    gen_server:cast(room, {msg, Nick, Payload})
            end,
            {noreply, State}
    end;

handle_info({tcp_closed,_}, State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Client received ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

