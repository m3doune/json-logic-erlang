-module(json_logic_test).

-include_lib("eunit/include/eunit.hrl").

empty_string_test() ->
    ?assertEqual(3.14, json_logic:apply("{\"var\": \"\"}", 3.14)).

map_access_test() ->
    ?assertEqual(3.14, json_logic:apply("{\"var\": \"pi\"}", #{<<"pi">> => 3.14})).

default_values_test() ->
    ?assertEqual(3.14, json_logic:apply("{\"var\": [\"pi\", 3.14]}", null)),
    ?assertEqual(26, json_logic:apply("{\"var\":[\"z\", 26]}", #{<<"a">> => 1, <<"b">> => 2})).

nested_value_test() ->
    Complex = #{
        <<"challenger">> => #{
            <<"height">> => 183,
            <<"name">> => <<"Dread Pirate Roberts">>},
        <<"champ">> => #{
            <<"height">> => 223,
            <<"name">> => <<"Fezzig">>
        }
    },
    ?assertEqual(<<"Fezzig">>, json_logic:apply("{\"var\" : \"champ.name\"}", Complex)),
    ?assertEqual(null, json_logic:apply("{\"var\" : \"unknown\"}", Complex)),
    ?assertEqual(null, json_logic:apply("{\"var\" : \"challenger.some_other_key\"}", Complex)),
    ?assertEqual(
        #{<<"height">> => 183, <<"name">> => <<"Dread Pirate Roberts">>},
        json_logic:apply("{\"var\" : \"challenger\"}", Complex)
    ).
    % ?debugFmt("JSON Map ~p~n",[jiffy:decode("{
    %     \"champ\" : {
    %       \"name\" : \"Fezzig\",
    %       \"height\" : 223
    %     },
    %     \"challenger\" : {
    %       \"name\" : \"Dread Pirate Roberts\",
    %       \"height\" : 183
    %     }
    %   }", [return_maps])]).