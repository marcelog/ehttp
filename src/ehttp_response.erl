-module(ehttp_response).

-type response_data() :: {atom(), term()}.
-type response() :: [response_data()].
-type code() :: binary().
-type status() :: binary().
-type data_length() :: integer() | chunked | unknown.

-export_type([response/0, code/0, status/0]).

-export([new_response/5, marshall/1, unmarshall/1, is_chunked_transfer/1]).
-export([get_headers/1, get_data_size/1]).

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
    [HttpVersion, Code,StatusHead|StatusTail] = ehttp_bin:split(
        ehttp_bin:trim_newline(Response)
    ),
    Status = lists:foldl(
        fun(Word, Acc) ->
            <<Acc/binary, " ", Word/binary>>
        end,
        StatusHead,
        StatusTail
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

%% @doc Returns the headers for this response.
-spec get_headers(Response::response()) -> ehttp_header:headers().
get_headers(Response) ->
    get_value(Response, headers).

%% @doc Returns true if the response contains a header "transfer-encoding" with
%% value "chunked".
-spec is_chunked_transfer(Response::response()) -> boolean().
is_chunked_transfer(Response) ->
    Headers = get_headers(Response),
    case ehttp_header:get_single(Headers, <<"transfer-encoding">>) of
        notfound -> false;
        Value -> ehttp_bin:lc(Value) =:= <<"chunked">>
    end.

%% @doc Returns the size of the contents, whenever possible.
-spec get_data_size(Response::response()) -> data_length().
get_data_size(Response) ->
    case is_chunked_transfer(Response) of
        true -> chunked;
        false -> get_length_by_header(Response)
    end.

%% @doc Returns the content of the header content-length as integer, or
%% unknown.
-spec get_length_by_header(Response::response()) -> notfound | integer().
get_length_by_header(Response) ->
    Headers = ehttp_response:get_headers(Response),
    case ehttp_header:get_single(Headers, <<"content-length">>) of
        notfound -> unknown;
        Value -> list_to_integer(binary_to_list(Value))
    end.