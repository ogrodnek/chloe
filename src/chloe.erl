-module(chloe).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(mnesia),
  application:start(?MODULE).

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(chloe_port),
    {ok, JID} = application:get_env(chloe_jid),
    {ok, Pass} = application:get_env(chloe_pass),
    chloe_sup:start_link(Port, JID, Pass).

stop(_State) ->
    ok.
