-module(message_router).
-behavior(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-export([send_message/4, register_nick/2, unregister_nick/1, shutdown/0]).


%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_message(Topic, Type, App, Message) ->
    gen_server:call(?SERVER, {send_msg, Topic, Type, App, Message}).

register_nick(ClientName, ClientPid) ->
    gen_server:call(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
    gen_server:call(?SERVER, {unregister_nick, ClientName}).

shutdown() ->
    gen_server:cast(?SERVER, stop).

% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    user_prefs:start_link(),
    {ok, dict:new()}.

handle_call({send_msg, Topic, Type, App, Message}, _From, Clients) ->
    Users = user_prefs:find_users(Topic, Type, App),
    io:format("Found users ~p~n", [Users]),
    F = fun(User) ->
                case dict:find(User, Clients) of
                    {ok, Pid} -> 
                        io:format("Sending message to ~p~n", [User]),
                        Pid ! { msg, Message };
                    error -> ok
                end
        end,
    lists:foreach(F, Users),
    {reply, ok, Clients};

handle_call({register_nick, ClientName, ClientPid}, _From, Clients) ->
    io:format("Registering ~p ~p ~n", [ClientName, ClientPid]),
    NewClients = dict:store(ClientName, ClientPid, Clients),
    {reply, ok, NewClients};

handle_call({unregister_nick, ClientName}, _From, Clients) ->
    NewClients = case dict:find(ClientName, Clients) of
                     {ok, ClientPid} ->
                         ClientPid ! stop,
                         dict:erase(ClientName, Clients);
                     error ->
                         io:format("Unknown client: ~p~n", [ClientName]),
                         Clients
                 end,
    {reply, ok, NewClients};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    user_prefs:shutdown(),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



