-module(ehttp_response).
-include_lib("eunit/include/eunit.hrl").

-type response_data() :: {atom(), term()}.
-type response() :: [response_data()].
-type code() :: binary().
-type status() :: binary().
-export_type([response/0, code/0, status/0]).

-export([new_response/5, marshall/1, unmarshall/1]).

%% @doc Returns a new response.
-spec new_response(
    Version::ehttp_http:version(), Code::code(), Status::status(),
    Headers::ehttp_header:headers(), Cookies::ehttp_header:cookies()
) -> response().
new_response(Version, Code, Status, Headers, Cookies)
    when is_atom(Version), is_binary(Code),
    is_binary(Status), is_list(Headers), is_list(Cookies) ->
    [
        {version, Version}, {code, Code}, {status, Status},
        {headers, Headers}, {cookies, Cookies}
    ].

%% @doc Unserializes a list of binaries, which should be a full response
%% without data, line by line.
-spec unmarshall(List::[binary()]) -> response().
unmarshall([Response | Headers]) ->
    [HttpVersion, Code, Status] = ehttp_bin:split(
        ehttp_bin:trim_newline(Response)
    ),
    Version = ehttp_http:unmarshall_version(binary:part(HttpVersion, {5, 3})),
    {NewHeaders, Cookies} = lists:foldl(
        fun({Key, Values}, {AccH, AccC}) ->
            case Key of
                <<"set-cookie">> ->
                    NewAccC = lists:foldl(
                        fun(ValBin, Acc) ->
                            ehttp_cookie:set(
                                Acc, ehttp_cookie:unmarshall(
                                    ehttp_bin:trim_newline(ValBin)
                                )
                            )
                        end,
                        AccC,
                        Values
                    ),
                    {AccH, NewAccC};
                _ -> {ehttp_header:set(AccH, Key, Values), AccC}
            end
        end,
        {ehttp_header:new_header_list(), ehttp_cookie:new_cookies()},
        ehttp_header:unmarshall(Headers)
    ),
    new_response(Version, Code, Status, NewHeaders, Cookies).

%% @doc Serializes a response, ready to be sent by wire.
-spec marshall(Response::response()) -> binary().
marshall(Response) when is_list(Response) ->
    Version = ehttp_http:marshall_version(get_value(Response, version)),
    Code = get_value(Response, code),
    Status = get_value(Response, status),
    Headers = get_value(Response, headers),
    Cookies = get_value(Response, cookies),
    ResBin = ehttp_http:marshall(
        <<"HTTP/", Version/binary, " ", Code/binary, " ", Status/binary>>
    ),
    HeadersBin = ehttp_header:marshall(Headers),
    CookiesBin = ehttp_cookie:marshall(Cookies, <<"set-cookie">>),
    ehttp_http:marshall(
        <<ResBin/binary, HeadersBin/binary, CookiesBin/binary>>
    ).

%% @doc Wrapper around lists:keyfind.
-spec get_value(List::[{atom(), binary()}], Key::atom()) -> binary().
get_value(List, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.
