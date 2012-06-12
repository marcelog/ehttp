%%% @doc Code needed to allocate, manipulate, marshall and unmarshall HTTP
%%% requests.
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
-module(ehttp_request).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-include_lib("eunit/include/eunit.hrl").

-export([new_request/6, new_request/3, marshall/1, unmarshall/1]).

-type path() :: binary().
-type request_data() :: {atom(), term()}.
-type request() :: [request_data()].
-export_type([path/0, request/0]).

%% @doc Returns a new request.
-spec new_request(
    Method::ehttp_http:method(), Path::string(),
    Version::ehttp_http:version(), Variables::ehttp_variable:variables(),
    Headers::ehttp_header:headers(), Cookies::ehttp_cookie:cookies()
) -> request().
new_request(Method, Path, Version, Variables, Headers, Cookies)
    when is_atom(Method), is_binary(Path), is_atom(Version),
    is_list(Variables), is_list(Headers), is_list(Cookies) ->
    [
        {method, Method}, {path, Path},
        {version, Version}, {req_vars, Variables},
        {headers, Headers}, {cookies, Cookies}
    ].

%% @doc Returns a new request, without specifying headers.
-spec new_request(
    Method::ehttp_http:method(), Path::string(), Version::ehttp_http:version()
) -> request().
new_request(Method, Path, Version)
    when is_atom(Method), is_binary(Path), is_atom(Version) ->
    new_request(
        Method, Path, Version, ehttp_variable:new_variables(),
        ehttp_header:new_header_list(), ehttp_cookie:new_cookies()
    ).

%% @doc Serializes a request, ready to be sent by wire.
-spec marshall(Request::request()) -> binary().
marshall(Request) when is_list(Request) ->
    Method = ehttp_http:marshall_method(get_value(Request, method)),
    Path = get_value(Request, path),
    Version = ehttp_http:marshall_version(get_value(Request, version)),
    Headers = get_value(Request, headers),
    Cookies = get_value(Request, cookies),
    ReqVars = get_value(Request, req_vars),

    VarsBin = case marshall_req_variables(ReqVars) of
        [] -> <<>>;
        [FirstVar | []] -> <<"?", FirstVar/binary>>;
        [FirstVar | Others] ->
            lists:foldl(
                fun(VarBin, Acc) ->
                    <<Acc/binary, "&", VarBin/binary>>
                end,
                <<"?", FirstVar/binary>>,
                Others
            )
        end,
    ReqBin = ehttp_http:marshall(
        <<Method/binary, " ", Path/binary,
        VarsBin/binary, " HTTP/", Version/binary>>
    ),
    HeadersBin = ehttp_header:marshall(Headers),
    CookiesBin = ehttp_cookie:marshall(Cookies, <<"cookie">>),
    ehttp_http:marshall(<<ReqBin/binary, HeadersBin/binary, CookiesBin/binary>>).

%% @doc Unmarshalls cookies in a binary, multiple cookies may be specified
%% separated by &lt;&lt;";"&gt;&gt;
-spec unmarshall_cookies_from_binary(
    Bin::binary(), Cookies::ehttp_cookie:cookies()
) -> binary().
unmarshall_cookies_from_binary(Bin, Cookies) when is_binary(Bin) ->
    lists:foldl(
        fun(CookieBin, Acc) ->
            Cookie = ehttp_cookie:unmarshall(CookieBin),
            ehttp_cookie:set(Acc, Cookie)
        end,
        Cookies,
        ehttp_bin:split_by_char(Bin, <<";">>, false)
    ).

%% @doc Unserializes a list of binaries, which should be a full request
%% without data, line by line.
-spec unmarshall(List::[binary()]) -> request().
unmarshall([Request | Headers]) ->
    [Method, Path, HttpVersion] = ehttp_bin:split(Request),
    Method2 = ehttp_http:unmarshall_method(ehttp_bin:lc(Method)),
    Version = ehttp_http:unmarshall_version(binary:part(HttpVersion, {5, 3})),
    {RealPath, VarsBin} = case ehttp_bin:split_by_char(Path, <<"?">>, true) of
        [P, V] -> {P, V};
        [P] -> {P, <<>>}
    end,
    Vars = ehttp_bin:parse_key_values_by(VarsBin, <<"&">>),
    UnmarshalledHeaders = ehttp_header:unmarshall(Headers),
    {NewHeaders, NewCookies} = lists:foldl(
        fun({Key, Values}, {AccH, AccC}) ->
            case Key of
                <<"cookie">> ->
                    NewAccC = lists:foldl(
                        fun(ValBin, Acc) ->
                            unmarshall_cookies_from_binary(ValBin, Acc)
                        end,
                        AccC,
                        Values
                    ),
                    {AccH, NewAccC};
                _ -> {ehttp_header:set(AccH, Key, Values), AccC}
            end
        end,
        {ehttp_header:new_header_list(), ehttp_cookie:new_cookies()},
        UnmarshalledHeaders
    ),
    new_request(Method2, RealPath, Version, Vars, NewHeaders, NewCookies).

%% @doc Marshalls all request variables.
-spec marshall_req_variables(Vars::ehttp_variable:variables()) -> binary().
marshall_req_variables(Vars) when is_list(Vars) ->
    lists:map(
        fun({Key, Value}) ->
            RealValue = ehttp_bin:encode(Value),
            <<Key/binary, "=", RealValue/binary>>
        end,
        Vars
    ).

%% @doc Wrapper around lists:keyfind.
-spec get_value(List::[{atom(), binary()}], Key::atom()) -> binary().
get_value(List, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.

