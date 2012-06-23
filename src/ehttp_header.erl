%%% @doc HTTP headers manipulation functions.
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
-module(ehttp_header).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%% Header manipulation functions.
-export([new_header_list/0, set/3, get/2, get_single/2, has/2]).
-export([marshall/1, unmarshall/1]).

%% Types.
-type header_key() :: ehttp_variable:variable_key().
-type header_value() :: ehttp_variable:variable_value().
-type header_values() :: [header_value()].
-type header() :: {header_key(), header_value()}.
-type headers() :: [header()].
-export_type([header_key/0, header_value/0, header/0, headers/0]).

%% @doc Returns a new header list.
-spec new_header_list() -> headers().
new_header_list() ->
    ehttp_variable:new_variables().

%% @doc Returns a serialized header, like Key: Value\r\n.
-spec marshall(Key::binary(), Value::binary()) -> binary().
marshall(Key, Value) ->
    ehttp_http:marshall(<<Key/binary, ":", Value/binary>>).

%% @doc Serializes all headers present in list, ready to be sent by wire.
-spec marshall(Headers::headers()) -> binary().
marshall(Headers) when is_list(Headers) ->
    lists:foldl(
        fun({Key, Values}, Acc) ->
            lists:foldl(
                fun (Value, InAcc) ->
                    Marshalled = marshall(Key, Value),
                    <<InAcc/binary, Marshalled/binary>>
                end,
                Acc,
                Values
            )
        end,
        <<>>,
        Headers
    ).

%% @doc Parses binaries exactly like: "Key: Value" and returns a header, with
%% its key lower-cased.
-spec unmarshall(Header::binary())  -> {header()}.
unmarshall(Header) when is_binary(Header) ->
    [<<Key/binary>>,<<Value/binary>>] = ehttp_bin:split_by_char(
        ehttp_bin:trim_newline(Header), <<":">>, true
    ),
    {ehttp_bin:trim(ehttp_bin:lc(Key)), ehttp_bin:trim(Value)};

unmarshall(HeadersBin) when is_list(HeadersBin) ->
    lists:foldl(
        fun(HeaderBin, Acc) ->
            {Header, Value} = unmarshall(HeaderBin),
            set(Acc, Header, Value)
        end,
        new_header_list(),
        HeadersBin
    ).

%% @doc Sets a header value. If the header does not exist, it will be added.
%% If the header already exists, the value will be replaced.
-spec set(
    Headers::headers(), Key::header_key(), Value::header_value()
) -> headers().
set(Headers, Key, Values) when is_list(Values) ->
    ehttp_variable:set(Headers, Key, Values);

set(Headers, Key, Value) ->
    NewValue = case get(Headers, Key) of
        notfound -> [Value];
        OldValues -> [Value|OldValues]
    end,
    ehttp_variable:set(Headers, Key, NewValue).

%% @doc Returns a header value. Will stop traversing the header list when
%% found.
-spec get(Headers::headers(), Key::header_key()) -> header_values() | notfound.
get(Headers, Key) ->
    ehttp_variable:get(Headers, Key).

%% @doc true if the given header list contains the given header.
-spec has(Headers::headers(), Key::header_key()) -> boolean().
has(Headers, Key) ->
    ehttp_variable:has(Headers, Key).

%% @doc Returns the first value found for the given key.
-spec get_single(
    Headers::headers(), Key::header_value()
) -> header_value().
get_single(Headers, Key) ->
    case get(Headers, Key) of
        notfound -> notfound;
        Val -> hd(Val)
    end.