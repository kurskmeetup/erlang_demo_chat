-module(room).
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
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({join,ClientName,ClientPID}, State) ->
    State2=State ++ [{ClientPID,ClientName}],
    erlang:monitor(process, ClientPID),
    send_message(<<"system">>,<<"Welcome, ",
        ClientName/binary>>,State2),
    {noreply, State2};

handle_cast({msg,ClientName,Message}, State) ->
    send_message(ClientName,Message,State),
    {noreply, State};


handle_cast(Msg, State) ->
    error_logger:warning_msg("Room message received ~p~n",[Msg]),
    {noreply, State}.

handle_info({'DOWN',_Ref,process,PID,_Reason}, State) ->
    State2=lists:keydelete(PID, 1, State), 
    case lists:keyfind(PID,1,State) of
        {PID, Nick} ->
            send_message(<<"system">>,<<"Bye, ", Nick/binary>>,State2),
            Nick;
        false -> 
            undefined
    end,
    {noreply, State2};

handle_info(_Info, State) ->
    error_logger:warning_msg("Info ~p~n",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_message(From, Msg, Users) ->
    lists:foreach(fun({User,Nick}) ->
                gen_server:cast(User, {msg, From, Msg})
        end, Users).

