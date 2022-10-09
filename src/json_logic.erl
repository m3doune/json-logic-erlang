-module(json_logic).

-compile({no_auto_import, [apply/2]}).

-export([apply/2]).

-spec apply(Logic :: map() | null, Data :: map() | null) -> any().
apply(Logic, Data) when is_list(Logic) ->
    apply(jiffy:decode(Logic, [return_maps]), Data);
apply(#{} = Logic, Data) when map_size(Logic) =/= 0 andalso map_size(Logic) == 1 ->
    [Operation] = maps:keys(Logic),
    Values = maps:get(Operation, Logic),
    io:format("Format ~p~n", [Values]),
    case catch operations:(binary_to_atom(Operation, utf8))({Values, Data}) of
        {'EXIT', Reason} ->
            {error, Reason};
        Result ->
            Result
    end;
apply(Privitive_value, _) ->
    Privitive_value.