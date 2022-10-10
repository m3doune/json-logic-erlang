-module(json_logic).

-compile({no_auto_import, [apply/2]}).

-include("../include/json_logic.hrl").

-export([apply/2]).
-export([truthy/1]).

-spec apply(Logic :: map() | null, Data :: map() | null) -> any().
apply(Logic, Data) when is_list(Logic) ->
    apply(jiffy:decode(Logic, [return_maps]), Data);
apply(#{} = Logic, Data) when ?IS_LOGIC(Logic) ->
    [Operation] = maps:keys(Logic),
    Values = maps:get(Operation, Logic),
    case catch operations:handle(Operation, {Values, Data}) of
        {'EXIT', Reason} ->
            {error, Reason};
        Result ->
            Result
    end;
apply(Privitive_value, _) ->
    Privitive_value.

-spec truthy(Arg :: any()) -> boolean().
truthy(0) -> false;
truthy([]) -> false;
truthy(null) -> false;
truthy(_) -> true.