-module(header_test).
-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

can_set_and_get(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    [?_assertEqual(<<"value">>, ehttp_header:get_single(H2, <<"key">>))].

can_set_and_get_multiple(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    H3 = ehttp_header:set(H2, <<"key2">>, <<"value2">>),
    [?_assertEqual(<<"value">>, ehttp_header:get_single(H3, <<"key">>)),
    ?_assertEqual(<<"value2">>, ehttp_header:get_single(H3, <<"key2">>))
    ].

cannot_get_unknown(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    [?_assertEqual(notfound, ehttp_header:get_single(H1, <<"key">>)),
    ?_assertEqual(notfound, ehttp_header:get(H1, <<"key">>))].

can_add_value(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    H3 = ehttp_header:set(H2, <<"key">>, <<"value3">>),
    H4 = ehttp_header:set(H3, <<"key2">>, <<"value2">>),
    H5 = ehttp_header:set(H4, <<"key2">>, <<"value4">>),
    [?_assertEqual([<<"value3">>,<<"value">>], ehttp_header:get(H5, <<"key">>)),
    ?_assertEqual([<<"value4">>,<<"value2">>], ehttp_header:get(H5, <<"key2">>))
    ].

can_marshall(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    H3 = ehttp_header:set(H2, <<"key2">>, <<"value2">>),
    Data = <<"key:value\r\nkey2:value2\r\n">>,
    [?_assertEqual(Data, ehttp_header:marshall(H3))].

can_know_if_has_header(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    [?_assertNot(ehttp_header:has(H1, <<"key">>)),
    ?_assert(ehttp_header:has(H2, <<"key">>))].

can_unmarshall(_SetupData) ->
    Data = <<"  \tKEy2  \t\t : value2  \t">>,
    [?_assertEqual({<<"key2">>,<<"value2">>}, ehttp_header:unmarshall(Data))].

can_unmarshall_multiple(_SetupData) ->
    Data = [
        <<"  \tKEy1  \t\t : value1  \t">>,
        <<"  \tKEy2  \t\t : value2  \t">>
    ],
    [?_assertEqual(
        [{<<"key1">>,[<<"value1">>]}, {<<"key2">>,[<<"value2">>]}],
        ehttp_header:unmarshall(Data)
    )].

ehttp_header_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_set_and_get(SetupData),
                can_set_and_get_multiple(SetupData),
                can_add_value(SetupData),
                cannot_get_unknown(SetupData),
                can_marshall(SetupData),
                can_unmarshall(SetupData),
                can_know_if_has_header(SetupData),
                can_unmarshall_multiple(SetupData)
            ]}
        end
    }.
