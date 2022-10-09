-module(operations).

-export([var/1]).

var({<<>>, Data}) ->
    Data;
var({[_, Default], null}) ->
    Default;
var({_, null}) ->
    null;
var({Key_list, #{} = Data}) ->
   {Key, Default} = case Key_list of
        [Some_key, Some_default_value]  ->
            {Some_key, Some_default_value};
        [_] ->
            {hd(Key_list), null};
        Binary when is_binary(Binary) ->
            {Binary, null}
    end,
    fetch_key_value(Key, Data, Default).

fetch_key_value([], Last_found, _) ->
    Last_found;
fetch_key_value(<<Char,_/binary>>= Key, Data, Default) when is_integer(Char) ->
    fetch_key_value(binary:split(Key, <<".">>), Data, Default);
fetch_key_value([Key | Keys], Data, Default) ->
    fetch_key_value(Keys, maps:get(Key, Data, Default), Default).
