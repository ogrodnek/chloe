-module(user_prefs).
-behavior(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(user_pref,
        {user,
         topic,
         type,
         app}).

%%% API
-export([start_link/0, save_pref/4, find_prefs/1, find_users/3, shutdown/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_pref(User, Topic, Type, App) ->
    gen_server:call(?SERVER, {save_pref, User, Topic, Type, App}).

find_prefs(User) ->
    {ok, Prefs} = gen_server:call(?SERVER, {find_prefs, User}),
    Prefs.

find_users(Topic, Type, App) ->
    io:format("find_users: ~p ~p ~p~n", [Topic, Type, App]),
    {ok, Users} = gen_server:call(?SERVER, {find_users, Topic, Type, App}),
    Users.

shutdown() ->
    gen_server:cast(?SERVER, stop).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) starting...~n", [?MODULE, self()]),
    init_store(),
    {ok, []}.

handle_call({save_pref, User, Topic, Type, App}, _From, State) ->
    store_pref(User, Topic, Type, App),
    {reply, ok, State};

handle_call({find_prefs, User}, _From, State) ->
    Prefs = get_prefs(User),
    {reply, {ok, Prefs}, State};

handle_call({find_users, Topic, Type, App}, _From, State) ->
    Users = get_users(Topic, Type, App),
    {reply, {ok, Users}, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    io:format("Terminating..."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal 

store_pref(User, Topic, Type, App) ->
    F = fun() ->
                mnesia:write(#user_pref{user=User, topic=Topic, type=Type, app=App})
        end,
    mnesia:transaction(F).

get_prefs(User) ->
    F = fun() ->
                Query = qlc:q([P || P <- mnesia:table(user_pref),
                                    P#user_pref.user =:= User]),
                qlc:e(Query)
        end,
    {atomic, Prefs} = mnesia:transaction(F),
    Prefs.

get_users(Topic, Type, App) ->
    F = fun() ->
                Query = qlc:q([P#user_pref.user || P <- mnesia:table(user_pref),
                                                   ((P#user_pref.topic =:= Topic) or (P#user_pref.topic =:= "*"))
                                                       and ((P#user_pref.type =:= Type) or (P#user_pref.type =:= "*"))
                                                       and ((P#user_pref.app =:= App) or (P#user_pref.app =:= "*"))]),
                qlc:e(Query)
        end,
    {atomic, Users} = mnesia:transaction(F),
    Users.


init_store() ->
    mnesia:create_schema([node()]),
    try
        mnesia:table_info(user_pref, type)
    catch
        exit: _ ->
            mnesia:create_table(user_pref, [{attributes, record_info(fields, user_pref)},
                                            {type, bag},
                                            {ram_copies, [node()]}])
    end.
