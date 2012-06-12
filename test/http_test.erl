-module(http_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

can_marshall(_SetupData) ->
    [?_assertEqual(<<"asd\r\n">>, ehttp_http:marshall(<<"asd">>))].

can_unmarshall(_SetupData) ->
    [
        ?_assertEqual(<<"asd">>, ehttp_http:unmarshall(<<"asd\r\n">>)),
        ?_assertEqual(<<"asd">>, ehttp_http:unmarshall(<<"asd\n">>)),
        ?_assertEqual(<<"asd">>, ehttp_http:unmarshall(<<"asd\r">>)),
        ?_assertEqual(<<>>, ehttp_http:unmarshall(<<"\r\n">>))
    ].

ehttp_http_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_marshall(SetupData),
                can_unmarshall(SetupData)
            ]}
        end
    }.
