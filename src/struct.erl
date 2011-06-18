-module(struct).

-export([get_value/2]).

get_value(Path, Struct) when is_tuple(Path) ->
	L = tuple_to_list(Path),
	get_val(L, Struct);
get_value(Key, Struct) ->
	{struct, L} = Struct,
	proplists:get_value(Key, L).

get_val(_, undefined) ->
	undefined;
get_val([Key], Struct) ->
	get_value(Key, Struct);
get_val([Key | T], Struct) ->
	NewStruct = get_value(Key, Struct),
	get_val(T, NewStruct).
