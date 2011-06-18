-module(sns_web).

-behavior(gen_server).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).

% api
-export([start_link/1, dispatch_requests/1, shutdown/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([sample_sns/0, sample_cowboy/0]).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    mochiweb_http:start([{port, Port},
                         {loop, fun dispatch_requests/1}]),
    erlang:monitor(process, mochiweb_http),
    {ok, []}.

shutdown() ->
    gen_server:cast(?SERVER, stop).

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, {mochiweb_http, _}, _}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


dispatch_requests(Req) ->
    {struct, Data} = mochijson2:decode(Req:recv_body()),
    Sns = sns_message:new(Data),
    handle_sns(Sns:get_type(), Sns),
    io:format("Type is: ~p~n", [Sns:get_type()]),
    Req:ok({"text/plain", ?OK}).


% Internal

handle_sns(<<"Notification">>, Sns) ->
    D = mochijson2:decode(Sns:get_message()),
    Subject = binary_to_list(Sns:get_subject()),
    Topic = Sns:get_topic(),
    Type = binary_to_list(struct:get_value({<<"meta">>, <<"type">>}, D)),
    App = binary_to_list(struct:get_value({<<"cowboy-metadata">>, <<"application">>}, D)),
    io:format("Got message ~p ~p ~p ~p~n", [Topic, Type, App, Subject]),
    message_router:send_message(Topic, Type, App, Subject),
    ok;

handle_sns(<<"SubscriptionConfirmation">>, Sns) ->
    io:format("Subscribing to topic: ~p~n", [Sns:get_topic()]),
    Url=binary_to_list(Sns:get_subscribe_url()),
    httpc:request(get, {Url, []}, [], []),
    io:format("Subscribed to topic: ~p~n", [Sns:get_topic()]),
    ok.
