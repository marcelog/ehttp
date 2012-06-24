-module(response_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

can_marshall(_SetupData) ->
    Req = <<"HTTP/1.1 302 Found\r\n",
        "location:http://www.domain.com/\r\n",
        "cache-control:private\r\n",
        "content-type:text/html; charset=UTF-8\r\n",
        "date:Fri, 08 Jun 2012 14:27:09 GMT\r\n",
        "server:gws\r\n",
        "content-length:222\r\n",
        "x-xss-protection:1; mode=block\r\n",
        "x-frame-options:SAMEORIGIN\r\n",
        "set-cookie:key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com\r\n",
        "set-cookie:key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly\r\n\r\n">>,
    Data = [{version,http_v11},
        {code,<<"302">>},
        {status,<<"Found">>},
        {headers,[{<<"location">>, [<<"http://www.domain.com/">>]},
            {<<"cache-control">>, [<<"private">>]},
            {<<"content-type">>, [<<"text/html; charset=UTF-8">>]},
            {<<"date">>, [<<"Fri, 08 Jun 2012 14:27:09 GMT">>]},
            {<<"server">>, [<<"gws">>]},
            {<<"content-length">>, [<<"222">>]},
            {<<"x-xss-protection">>, [<<"1; mode=block">>]},
            {<<"x-frame-options">>, [<<"SAMEORIGIN">>]}
        ]},
        {cookies, [
            {<<"key">>, {
                {name, <<"key">>}, {value, <<"value:a:b">>},
                {params, [
                    {<<"expires">>,<<"Sun, 08-Jun-2014 14:27:09 GMT">>},
                    {<<"path">>,<<"/">>},
                    {<<"domain">>,<<".domain.com">>}
                ]}
            }},
            {<<"key2">>, {
                {name, <<"key2">>}, {value, <<"value2:a:b">>},
                {params, [
                    {<<"expires">>,<<"Sat, 08-Dec-2012 14:27:09 GMT">>},
                    {<<"path">>,<<"/">>},
                    {<<"domain">>,<<".domain.com">>},
                    {<<"HttpOnly">>,<<>>}
                ]}
            }}
        ]}
    ],
    [?_assertEqual(Req, ehttp_response:marshall(Data))].

can_unmarshall(_SetupData) ->
    Data = [
        <<"HTTP/1.1 302 Found and this is the tail\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"Content-Length: 222\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp = [{version,http_v11},
        {code,<<"302">>},
        {status,<<"Found and this is the tail">>},
        {headers,[{<<"location">>, [<<"http://www.domain.com/">>]},
            {<<"cache-control">>, [<<"private">>]},
            {<<"content-type">>, [<<"text/html; charset=UTF-8">>]},
            {<<"date">>, [<<"Fri, 08 Jun 2012 14:27:09 GMT">>]},
            {<<"server">>, [<<"gws">>]},
            {<<"content-length">>, [<<"222">>]},
            {<<"x-xss-protection">>, [<<"1; mode=block">>]},
            {<<"x-frame-options">>, [<<"SAMEORIGIN">>]}
        ]},
        {cookies, [
            {<<"key2">>, {
                {name, <<"key2">>}, {value, <<"value2:a:b">>},
                {params, [
                    {<<"expires">>,<<"Sat, 08-Dec-2012 14:27:09 GMT">>},
                    {<<"path">>,<<"/">>},
                    {<<"domain">>,<<".domain.com">>},
                    {<<"httponly">>,<<>>}
                ]}
            }},
            {<<"key">>, {
                {name, <<"key">>}, {value, <<"value:a:b">>},
                {params, [
                    {<<"expires">>,<<"Sun, 08-Jun-2014 14:27:09 GMT">>},
                    {<<"path">>,<<"/">>},
                    {<<"domain">>,<<".domain.com">>}
                ]}
            }}
        ]}
    ],
    [?_assertEqual(Resp, ehttp_response:unmarshall(Data))].

can_is_chunked_transfer(_SetupData) ->
    Data1 = [
        <<"HTTP/1.1 302 Found\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp1 = ehttp_response:unmarshall(Data1),
    Data2 = [
        <<"HTTP/1.1 302 Found\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Transfer-Encoding: Chunked\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp2 = ehttp_response:unmarshall(Data2),
    [?_assertEqual(false, ehttp_response:is_chunked_transfer(Resp1)),
    ?_assertEqual(true, ehttp_response:is_chunked_transfer(Resp2))].

can_return_content_length(_SetupData) ->
    Data1 = [
        <<"HTTP/1.1 302 Found\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp1 = ehttp_response:unmarshall(Data1),
    Data2 = [
        <<"HTTP/1.1 302 Found\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Transfer-Encoding: Chunked\r\n">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp2 = ehttp_response:unmarshall(Data2),
    Data3 = [
        <<"HTTP/1.1 302 Found\r\n">>,
        <<"Location: http://www.domain.com/\r\n">>,
        <<"Cache-Control: private\r\n">>,
        <<"Content-Type: text/html; charset=UTF-8\r\n">>,
        <<"Content-Length: 222\r\n">>,
        <<"Set-Cookie: key=value:a:b; expires=Sun, 08-Jun-2014 14:27:09 GMT; path=/; domain=.domain.com">>,
        <<"Set-Cookie: key2=value2:a:b; expires=Sat, 08-Dec-2012 14:27:09 GMT; path=/; domain=.domain.com; HttpOnly">>,
        <<"Date: Fri, 08 Jun 2012 14:27:09 GMT\r\n">>,
        <<"Server: gws\r\n">>,
        <<"X-XSS-Protection: 1; mode=block\r\n">>,
        <<"X-Frame-Options: SAMEORIGIN\r\n">>
    ],
    Resp3 = ehttp_response:unmarshall(Data3),
    [?_assertEqual(unknown, ehttp_response:get_data_size(Resp1)),
    ?_assertEqual(chunked, ehttp_response:get_data_size(Resp2)),
    ?_assertEqual(222, ehttp_response:get_data_size(Resp3))].

ehttp_response_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_marshall(SetupData),
                can_unmarshall(SetupData),
                can_is_chunked_transfer(SetupData),
                can_return_content_length(SetupData)
            ]}
        end
    }.
