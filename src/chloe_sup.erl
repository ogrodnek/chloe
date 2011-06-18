
-module(chloe_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(WORKER(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port, JID, Password) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, JID, Password]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, JID, Password]) ->
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    
    UserPrefs = ?WORKER(user_prefs, []),
    MessageRouter = ?WORKER(message_router, []),
    Jabber = ?WORKER(sns_jabber, [JID, Password]),
    WebServer = ?WORKER(sns_web, [Port]),
    {ok, {{one_for_all, 10, 30}, [UserPrefs, MessageRouter, Jabber, WebServer]}}.
    
