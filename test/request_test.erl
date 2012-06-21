-module(request_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

can_marshall_one_variable(_SetupData) ->
    Vars = ehttp_variable:new_variables(),
    V1 = ehttp_variable:set(Vars, <<"name1">>, <<"hello world">>),
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    Req = ehttp_request:new_request(
        ehttp_http:http_get(), <<"/index.html">>, ehttp_http:http_v11(),
        V1, H2, ehttp_cookie:new_cookies()
    ),
    Data = <<"GET /index.html?name1=hello%20world HTTP/1.1\r\n",
        "key:value\r\n\r\n">>,
    [?_assertEqual(Data, ehttp_request:marshall(Req))].

can_marshall_many_variables(_SetupData) ->
    Vars = ehttp_variable:new_variables(),
    V1 = ehttp_variable:set(Vars, <<"name1">>, <<"hello world">>),
    V2 = ehttp_variable:set(V1, <<"name2">>, <<"othervalue">>),
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    Req = ehttp_request:new_request(
        ehttp_http:http_get(), <<"/index.html">>, ehttp_http:http_v11(),
        V2, H2, ehttp_cookie:new_cookies()
    ),
    Data = <<"GET /index.html?name1=hello%20world&name2=othervalue"
        " HTTP/1.1\r\nkey:value\r\n\r\n">>,
    [?_assertEqual(Data, ehttp_request:marshall(Req))].

can_marshall(_SetupData) ->
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    Req = ehttp_request:new_request(
        ehttp_http:http_get(), <<"/index.html">>, ehttp_http:http_v11(), [], H2,
        ehttp_cookie:new_cookies()
    ),
    Data = <<"GET /index.html HTTP/1.1\r\nkey:value\r\n\r\n">>,
    [?_assertEqual(Data, ehttp_request:marshall(Req))].

can_use_different_versions(_SetupData) ->
    Method = ehttp_http:http_get(),

    Req1 = ehttp_request:new_request(
        Method, <<"/index.html">>, ehttp_http:http_v11()
    ),
    Data1 = <<"GET /index.html HTTP/1.1\r\n\r\n">>,

    Req2 = ehttp_request:new_request(
        Method, <<"/index.html">>, ehttp_http:http_v10()
    ),
    Data2 = <<"GET /index.html HTTP/1.0\r\n\r\n">>,

    Req3 = ehttp_request:new_request(
        Method, <<"/index.html">>, ehttp_http:http_v09()
    ),
    Data3 = <<"GET /index.html HTTP/0.9\r\n\r\n">>,
    [?_assertEqual(Data1, ehttp_request:marshall(Req1)),
    ?_assertEqual(Data2, ehttp_request:marshall(Req2)),
    ?_assertEqual(Data3, ehttp_request:marshall(Req3))].

can_use_different_methods_generate(Method, MethodAtom) ->
    Ver = ehttp_http:http_v11(),
    Req = ehttp_request:new_request(MethodAtom, <<"/index.html">>, Ver),
    Data = <<Method/binary, " /index.html HTTP/1.1\r\n\r\n">>,
    ?_assertEqual(Data, ehttp_request:marshall(Req)).

can_use_different_methods(_SetupData) ->
    [
        can_use_different_methods_generate(<<"POST">>, http_post),
        can_use_different_methods_generate(<<"GET">>, http_get),
        can_use_different_methods_generate(<<"HEAD">>, http_head),
        can_use_different_methods_generate(<<"OPTIONS">>, http_options),
        can_use_different_methods_generate(<<"PUT">>, http_put),
        can_use_different_methods_generate(<<"DELETE">>, http_delete),
        can_use_different_methods_generate(<<"TRACE">>, http_trace),
        can_use_different_methods_generate(<<"CONNECT">>, http_connect)
    ].

can_unmarshall_generate(Method, MethodAtom, Version, VersionAtom) ->
    Data = [
    <<"\t",Method/binary," \t\t /index.html \t\t  HTTP/",Version/binary,"  ">>,
    <<"Host: localhost">>,
    <<"Content-Type: text/xml; charset=utf-8">>,
    <<"Content-Length: length">>
    ],
    Req = ehttp_request:unmarshall(Data),
    Unmarshalled = [
        {method, MethodAtom}, {path, <<"/index.html">>}, {version, VersionAtom},
        {req_vars, []},
        {headers, [
            {<<"host">>, [<<"localhost">>]},
            {<<"content-type">>, [<<"text/xml; charset=utf-8">>]},
            {<<"content-length">>, [<<"length">>]}
        ]},
        {cookies, []}
    ],
    ?_assertEqual(Unmarshalled, Req).

can_unmarshall_methods(_SetupData) ->
    [
        can_unmarshall_generate(<<"POST">>, http_post, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"GET">>, http_get, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"HEAD">>, http_head, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"PUT">>, http_put, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"DELETE">>, http_delete, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"OPTIONS">>, http_options,<<"1.1">>,http_v11),
        can_unmarshall_generate(<<"TRACE">>, http_trace, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"CONNECT">>, http_connect, <<"1.1">>, http_v11)
    ].

can_unmarshall_versions(_SetupData) ->
    [
        can_unmarshall_generate(<<"POST">>, http_post, <<"1.1">>, http_v11),
        can_unmarshall_generate(<<"GET">>, http_get, <<"1.0">>, http_v10),
        can_unmarshall_generate(<<"HEAD">>, http_head, <<"0.9">>, http_v09)
    ].

can_unmarshall_variables(_SetupData) ->
    Data = [
    <<"GET /index.html?a=b&c=hello%20world HTTP/1.1">>,
    <<"Host: localhost">>,
    <<"Content-Type: text/xml; charset=utf-8">>,
    <<"Content-Length: length">>
    ],
    Req = ehttp_request:unmarshall(Data),
    Unmarshalled = [
        {method, http_get}, {path, <<"/index.html">>}, {version, http_v11},
        {req_vars, [{<<"a">>, <<"b">>}, {<<"c">>, <<"hello world">>}]},
        {headers, [
            {<<"host">>, [<<"localhost">>]},
            {<<"content-type">>, [<<"text/xml; charset=utf-8">>]},
            {<<"content-length">>, [<<"length">>]}
        ]},
        {cookies, []}
    ],
    ?_assertEqual(Unmarshalled, Req).

can_marshall_cookies(_SetupData) ->
    Vars = ehttp_variable:new_variables(),
    V1 = ehttp_variable:set(Vars, <<"name1">>, <<"hello world">>),
    H1 = ehttp_header:new_header_list(),
    H2 = ehttp_header:set(H1, <<"key">>, <<"value">>),
    C1 = ehttp_cookie:new_cookies(),
    C2 = ehttp_cookie:set(
        C1, <<"PREF">>,
        <<"ID=aaee33cc:U=asd123:FF=6:TM=122332:LM=122331:GM=7:S=asdqwe-asd">>
    ),
    C3 = ehttp_cookie:set(C2, <<"NID">>, <<"asdqwe123">>),
    Req = ehttp_request:new_request(
        ehttp_http:http_get(), <<"/index.html">>, ehttp_http:http_v11(),
        V1, H2, C3
    ),
    Data =
        <<"GET /index.html?name1=hello%20world HTTP/1.1\r\n",
        "key:value\r\n",
        "cookie:PREF=ID=aaee33cc:U=asd123:FF=6:TM=122332:LM=122331:GM=7:S=asdqwe-asd\r\n",
        "cookie:NID=asdqwe123\r\n\r\n">>,
    [?_assertEqual(Data, ehttp_request:marshall(Req))].

can_unmarshall_multiple_cookies_in_one_header(_SetupData) ->
    C1 = ehttp_cookie:new_cookies(),
    C2 = ehttp_cookie:set(C1, <<"a">>, <<"b">>),
    C3 = ehttp_cookie:set(C2, <<"c">>, <<"d">>),
    Req = ehttp_request:new_request(
        ehttp_http:http_get(), <<"/index.html">>, ehttp_http:http_v11(), [],
        ehttp_header:new_header_list(), C3
    ),
    Data = [<<"GET /index.html HTTP/1.1">>,<<"cookie:a=b;c=d">>],
    [?_assertEqual(Req, ehttp_request:unmarshall(Data))].

can_get_host_port_path(_SetupData) ->
    Data1 = [<<"GET http://host.com:8080/index.html HTTP/1.1">>, <<"key:value">>],
    Req1 = ehttp_request:unmarshall(Data1),

    Data2 = [<<"GET http://host.com/index.html HTTP/1.1">>, <<"key:value">>],
    Req2 = ehttp_request:unmarshall(Data2),

    Data3 = [<<"GET index.html HTTP/1.1">>, <<"host:host.com:8080">>],
    Req3 = ehttp_request:unmarshall(Data3),

    Data4 = [<<"GET index.html HTTP/1.1">>, <<"host:host.com">>],
    Req4 = ehttp_request:unmarshall(Data4),

    Data5 = [<<"GET index.html HTTP/1.1">>],
    Req5 = ehttp_request:unmarshall(Data5),
    [
        ?_assertEqual(
            {<<"host.com">>, 8080, <<"index.html">>},
            ehttp_request:get_host_port_path(Req1)
        ),
        ?_assertEqual(
            {<<"host.com">>, 80, <<"index.html">>},
            ehttp_request:get_host_port_path(Req2)
        ),
        ?_assertEqual(
            {<<"host.com">>, 8080, <<"index.html">>},
            ehttp_request:get_host_port_path(Req3)
        ),
        ?_assertEqual(
            {<<"host.com">>, 80, <<"index.html">>},
            ehttp_request:get_host_port_path(Req4)
        ),
        ?_assertEqual(
            {unknown, unknown, <<"index.html">>},
            ehttp_request:get_host_port_path(Req5)
        )
    ].

ehttp_request_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_marshall(SetupData),
                can_marshall_many_variables(SetupData),
                can_marshall_one_variable(SetupData),
                can_use_different_versions(SetupData),
                can_use_different_methods(SetupData),
                can_unmarshall_methods(SetupData),
                can_unmarshall_versions(SetupData),
                can_unmarshall_variables(SetupData),
                can_unmarshall_multiple_cookies_in_one_header(SetupData),
                can_marshall_cookies(SetupData),
                can_get_host_port_path(SetupData)
            ]}
        end
    }.
