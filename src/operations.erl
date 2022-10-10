-module(operations).

-export([handle/2]).

handle(<<"var">>, {<<>>, Data}) ->
    Data;
handle(<<"var">>, {[_, Default], null}) ->
    Default;
handle(<<"var">>, {_, null}) ->
    null;
handle(<<"var">>, {Index, Data}) when is_integer(Index), is_list(Data), Index >= 0,Index < length(Data) ->
    lists:nth(Index + 1, Data); %% 0 based indices
handle(<<"var">>, {Index, _}) when is_integer(Index) ->
    null; %% out of bounds / invalid index
handle(<<"var">>, {Key_list, #{} = Data}) ->
   {Key, Default} = case Key_list of
        [Some_key, Some_default_value]  ->
            {Some_key, Some_default_value};
        [_] ->
            {hd(Key_list), null};
        Binary when is_binary(Binary) ->
            {Binary, null}
    end,
    fetch_key_value(Key, Data, Default);
handle(<<"missing">>, {Logic, #{} = Data}) when is_list(Logic)->
    lists:filter(
        fun(Key) -> false == maps:is_key(Key, Data) end,
        Logic
    );
handle(<<"missing_some">>, {[N, Keys], #{} = Data}) ->
    case handle(<<"missing">>, {Keys, Data}) of
        Found when length(Found) > N -> [];
        All -> All
    end;
handle(<<"==">>, {[A, A], _}) -> true;
handle(<<"==">>, {[_, _], _}) -> false;
handle(Unrecognised, _) ->
    Error = lists:flatten(io_lib:fwrite("Unrecognized operation ~s", [binary_to_list(Unrecognised)])),
    throw({error, Error}).

fetch_key_value([], Last_found, _) ->
    Last_found;
fetch_key_value(<<Char,_/binary>>= Key, Data, Default) when is_integer(Char) ->
    fetch_key_value(binary:split(Key, <<".">>), Data, Default);
fetch_key_value([Key | Keys], Data, Default) ->
    fetch_key_value(Keys, maps:get(Key, Data, Default), Default).
