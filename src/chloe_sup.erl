
-module(chloe_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type), {?CHILD(I, Type, [])}).

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
    
    UserPrefs = {user_prefs, {user_prefs, start_link, []},
                 permanent, 5000, worker, [user_prefs]},
    MessageRouter = {message_router, {message_router, start_link, []},
                     permanent, 5000, worker, [message_router]},
    Jabber = {sns_jabber, {sns_jabber, start_link, [JID, Password]},
              permanent, 5000, worker, [sns_jabber]},
    WebServer = {sns_web, {sns_web, start_link, [Port]},
                 permanent, 5000, worker, [sns_web]},
    {ok, {{one_for_all, 10, 30}, [UserPrefs, MessageRouter, Jabber, WebServer]}}.
    
