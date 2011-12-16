-module(sns_jabber).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% TODO: this should be pulled from passed-in JID
-define(USER_ID, <<"chloe@jabber.example.com/ctu">>).

% API
-export([start_link/2, shutdown/0]).

start_link(JID, Password) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [JID, Password], []).

shutdown() ->
    gen_server:cast({local, ?SERVER}, stop).

% gen_server callbacks
init([JID, Password]) ->
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    application:start(exmpp),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start_link(),
    %% Create XMPP ID (Session Key):
    [User, Server] = string:tokens(JID, "@"),
    MyJID = exmpp_jid:make(User, Server, User),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(MySession, Server, 5222),

    exmpp_session:login(MySession),
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
                              exmpp_presence:set_status(
                                exmpp_presence:available(), "Ready for action")),
    {ok, MySession}.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, Session) ->
    exmpp_session:stop(Session),
    {stop, normal, Session};

handle_cast(_Msg, State) ->
    io:format("Got cast ~p", [_Msg]),
    {noreply, State}.

handle_info(#received_packet{packet_type=message, raw_packet=Message, type_attr=Type}, Session) when Type =/= "error" ->
    io:format("~p~n", [Message]),
    process_message(Session,  Message),
    {noreply, Session};

handle_info(#received_packet{packet_type=presence, raw_packet=Precense, type_attr=Type}, Session) when Type =/= "error" ->
    process_prescense(Session, Type, Precense),
    {noreply, Session};

handle_info(_Info, State) ->
    io:format("Got info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminating....~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal

process_prescense(Session, "available", Message) ->
    From = exmpp_jid:parse(exmpp_stanza:get_sender(Message)),
    {jid, Handle, Name, _, _} = From,
    Pid = spawn(fun() -> proxy_client(Session, Handle) end),
    message_router:register_nick(binary_to_list(Name), Pid),
    ok;

process_prescense(_Session, "unavailable", Message) ->
    From = exmpp_jid:parse(exmpp_stanza:get_sender(Message)),
    {jid, _, Name, _, _} = From,
    message_router:unregister_nick(binary_to_list(Name)),
    ok.


process_message(Session, Message) ->
    {jid, _, From, _, _} = exmpp_jid:parse(exmpp_stanza:get_sender(Message)),
    X = exmpp_xml:get_attribute(Message, <<"from">>, <<"unknown">>),
    Y = exmpp_xml:get_attribute(Message, <<"to">>, <<"unknown">>),
    io:format("~p ~p ~n", [X, Y]),
    case exmpp_message:get_body(Message) of 
        undefined -> ok;
        Body -> io:format("Got message from ~p, ~p~n", [From, Body]),
                Cmd = string:tokens(binary_to_list(Body), " "),
                io:format("Command is ~p~n", [Cmd]),
                Res = process_cmd(From, Cmd),
                Reply = exmpp_message:set_body(exmpp_stanza:reply_without_content(Message), Res),
                exmpp_session:send_packet(Session, Reply)

    end,
    ok.

process_cmd(User, ["list"]) ->
    case user_prefs:find_prefs(binary_to_list(User)) of
        [] -> "No subscriptions listed.  Use add endpoint/type/app to register.";
        Prefs -> subst("~p", Prefs)
    end;

process_cmd(User1, ["add" | [Topic]]) ->
    User = binary_to_list(User1),
    user_prefs:save_pref(User, Topic),
    io:format("Saving ~p ~p~n", [User, Topic]),
    subst("Okay, adding ~p....~n", [Topic]);

process_cmd(User, ["hi"]) ->
    subst("Hello, ~p", [binary_to_list(User)]);

process_cmd(_User, [Cmd | _]) ->
    subst("Unknown cmd ~p....~n", [Cmd]).

subst(Template, Values) when is_list(Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

% Client proxy process
proxy_client(Session, Client) ->
    receive
        {msg, Message } ->
            io:format("~p got message: ~p", [Client, Message]),
            Reply1 = exmpp_message:chat(Message),
            Reply2 = exmpp_stanza:set_recipient(Reply1, Client),
            Reply = exmpp_stanza:set_sender(Reply2, ?USER_ID),
            exmpp_session:send_packet(Session, Reply),
            proxy_client(Session, Client);
        stop ->
            io:format("~p proxy stopping~n", [Client]),
            ok
        end.
