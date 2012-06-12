%%% @doc Cookie handling functions.
%%%
%%% Copyright 2012 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(ehttp_cookie).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-type cookie_params() :: ehttp_variable:variables().
-type cookie_name() :: ehttp_variable:variable_key().
-type cookie_value() :: ehttp_variable:variable_value().
-type cookie() :: {
    {name, cookie_name()}, {value, cookie_value()}, {params, cookie_params()}
}.
-type cookies() :: ehttp_variable:variables().
-export_type([cookie_params/0, cookie_name/0, cookie_value/0, cookie/0]).

-export([has/2, get/2, set/2, set/3, marshall/2, unmarshall/1]).
-export([new_cookies/0, new_cookie/3, new_cookie/2]).

%% @doc Returns a new cookie jar.
-spec new_cookies() -> cookies().
new_cookies() ->
    ehttp_variable:new_variables().

%% @doc Creates a new cookie.
-spec new_cookie(
    Name::cookie_name(), Value::cookie_value(), Params::cookie_params()
) -> cookie().
new_cookie(Name, Value, Params) ->
    {{name, Name}, {value, Value}, {params, Params}}.

%% @doc Creates a new cookie with empty params.
-spec new_cookie(Name::cookie_name(), Value::cookie_value()) -> cookie().
new_cookie(Name, Value) ->
    new_cookie(Name, Value, ehttp_variable:new_variables()).

%% @doc Marshalls a cookie.
-spec marshall(
    Cookie::cookie()|cookies(), HeaderName::ehttp_header:header_name()
) -> binary().
marshall(
    {{name, Name}, {value, Value}, {params, Params}}=_Cookie,
    HeaderName
) when is_binary(HeaderName) ->
    Val = marshall_param(Name, Value),
    ParamsVal = marshall_params(Params),
    <<Val/binary, ParamsVal/binary>>;

%% @doc Marshalls a set of cookies.
marshall(Cookies, HeaderName) when is_list(Cookies), is_binary(HeaderName) ->
    lists:foldl(
        fun ({_Name, Cookie}, Acc) ->
            Bin = ehttp_http:marshall(marshall(Cookie, HeaderName)),
            NewCookie = <<HeaderName/binary, ":", Bin/binary>>,
            <<Acc/binary, NewCookie/binary>>
        end,
        <<>>,
        Cookies
    ).

%% @doc Marshalls a set of cookie parameters.
-spec marshall_params(Params::cookie_params()) -> binary().
marshall_params(Params) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            NewVal = marshall_param(Key, Value),
            <<Acc/binary, "; ", NewVal/binary>>
        end,
        <<>>,
        Params
    ).

%% @doc Marshalls a cookie parameter.
-spec marshall_param(Key::binary(), Value::binary()) -> binary().
marshall_param(Key, <<>>) when is_binary(Key) ->
    <<Key/binary>>;

marshall_param(Key, Value) when is_binary(Key), is_binary(Value) ->
    <<Key/binary, "=", Value/binary>>.

%% @doc Unmarshalls a cookie from a binary.
-spec unmarshall(Bin::binary()) -> cookie().
unmarshall(Bin) when is_binary(Bin) ->
    [NameValue|ParamList] = ehttp_bin:split_by_char(Bin, <<";">>, false),
    {Name, Value} = ehttp_bin:parse_key_equal_value(NameValue),
    Params = lists:foldl(
        fun(X, Acc) ->
            {Cname, Cvalue} =
                ehttp_bin:parse_key_equal_value(ehttp_bin:trim(X)),
            ehttp_variable:set(Acc, ehttp_bin:lc(Cname), Cvalue)
        end,
        ehttp_variable:new_variables(),
        ParamList
    ),
    new_cookie(ehttp_bin:trim(ehttp_bin:lc(Name)), Value, Params).

%% @doc Sets a cookie value. If the cookie does not exist, it will be added.
%% If the cookie already exists, the value will be replaced.
-spec set(
    Cookies::cookies(), Name::cookie_name(), Value::cookie_value()
) -> cookies().
set(Cookies, Name, Value) ->
    Cookie = new_cookie(Name, Value),
    ehttp_variable:set(Cookies, Name, Cookie).

%% @doc Adds a cookie to the given cookie list.
-spec set(Cookies::cookies(), Cookie::cookie()) -> cookies().
set(Cookies, {{name, Name}, {value, _Value}, {params, _Params}}=Cookie) ->
    ehttp_variable:set(Cookies, Name, Cookie).

%% @doc Returns a cookie value. Will stop traversing the cookie list when
%% found.
-spec get(Cookies::cookies(), Key::cookie_name()) -> cookie_value() | notfound.
get(Cookies, Key) ->
    ehttp_variable:get(Cookies, Key).

%% @doc true if the given cookie list contains the given cookie.
-spec has(Cookies::cookies(), Key::cookie_name()) -> boolean().
has(Cookies, Key) ->
    ehttp_variable:has(Cookies, Key).
