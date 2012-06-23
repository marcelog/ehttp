-module(cookie_test).

-include_lib("eunit/include/eunit.hrl").

start() ->
    [].

stop(_SetupData) ->
    ok.

can_get_cookie(_SetupData) ->
    Cookies1 = ehttp_cookie:new_cookies(),
    Cookies2 = ehttp_cookie:set(Cookies1, <<"a">>, <<"b">>),
    Cookies3 = ehttp_cookie:set(Cookies2, <<"b">>, <<"c">>),
    CookieTest = ehttp_cookie:new_cookie(<<"a">>, <<"b">>),
    [?_assertEqual(CookieTest, ehttp_cookie:get(Cookies3, <<"a">>))].

can_has_cookie(_SetupData) ->
    Cookies1 = ehttp_cookie:new_cookies(),
    Cookies2 = ehttp_cookie:set(Cookies1, <<"a">>, <<"b">>),
    Cookies3 = ehttp_cookie:set(Cookies2, <<"b">>, <<"c">>),
    [?_assertEqual(true, ehttp_cookie:has(Cookies3, <<"a">>))].

ehttp_request_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun(SetupData) ->
            {inparallel, [
                can_get_cookie(SetupData),
                can_has_cookie(SetupData)
            ]}
        end
    }.
