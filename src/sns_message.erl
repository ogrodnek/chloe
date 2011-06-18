-module(sns_message, [Sns]).

-export([get_type/0, get_message/0, get_topic/0, get_subject/0, get_subscribe_url/0]).

get_type() ->
    proplists:get_value(<<"Type">>, Sns).

get_message() ->
    proplists:get_value(<<"Message">>, Sns).

get_topic() ->
    Topic = binary_to_list(proplists:get_value(<<"TopicArn">>, Sns)),
    lists:last(string:tokens(Topic, ":")).

get_subject() ->
    proplists:get_value(<<"Subject">>, Sns).

get_subscribe_url() ->
    proplists:get_value(<<"SubscribeURL">>, Sns).
