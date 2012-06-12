%%% @doc Miscelaneous functions to handle binaries. Lots of these could be done
%%% with erlang's own functions.. I just wanted to have some fun :)
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
-module(ehttp_bin).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%% API exports.
-export([
    lc/1, trim_left/1, trim_right/1, trim/1, split/1, reverse/1,
    parse_key_equal_value/1, split_by_char/3,
    parse_key_values_by/2, encode/1, decode/1
]).

%% @doc Lowercases all characters between A-Z found in the binary.
-spec lc(Bin::binary()) -> binary().
lc(Bin) when is_binary(Bin) ->
    lc(Bin, <<>>).

lc(<<Byte:8, Rest/binary>>, Acc) ->
    NewByte = if
        Byte >= 65, Byte =< 90 -> Byte + 32;
        true -> Byte
    end,
    lc(Rest, <<Acc/binary, NewByte>>);

lc(<<>>, Acc) ->
    Acc.

%% @doc Removes " " and "\t" from left and right of the binary.
-spec trim(Bin::binary()) -> binary().
trim(Bin) when is_binary(Bin) ->
    trim_left(trim_right(Bin)).

%% @doc Removes " " and "\t" from the left of the binary.
-spec trim_left(Bin::binary()) -> binary().
%trim_left(<<>>) ->
%    <<>>;

trim_left(<<" ", Rest/binary>>) ->
    trim_left(Rest);

trim_left(<<"\t", Rest/binary>>) ->
    trim_left(Rest);

trim_left(Bin) when is_binary(Bin) ->
    Bin.

%% @doc Removes " " and "\t" from the right of the binary.
-spec trim_right(Bin::binary()) -> binary().
%trim_right(<<>>) ->
%    <<>>;

trim_right(Bin) when is_binary(Bin) ->
    case binary:last(Bin) of
        32 -> trim_right(binary:part(Bin, 0, byte_size(Bin) - 1));
        9 -> trim_right(binary:part(Bin, 0, byte_size(Bin) - 1));
        _ -> Bin
    end.

%% @doc Splits a binary by " " and "\t".
-spec split(Bin::binary()) -> [binary()].
split(Bin) when is_binary(Bin) ->
    reverse(split(Bin, [], <<>>)).

split(<<" ", Rest/binary>>, AccList, AccWord) ->
    NewAccList = split_add(AccWord, AccList),
    split(Rest, NewAccList, <<>>);

split(<<"\t", Rest/binary>>, AccList, AccWord) ->
    NewAccList = split_add(AccWord, AccList),
    split(Rest, NewAccList, <<>>);

split(<<>>, AccList, AccWord) ->
    split_add(AccWord, AccList);

split(<<Byte:8, Rest/binary>>, AccList, AccWord) ->
    NewAccWord = <<AccWord/binary, Byte>>,
    split(Rest, AccList, NewAccWord).

split_add(<<>>, AccList) ->
    AccList;

split_add(Bin, AccList) ->
    [Bin|AccList].

%% @doc Splits a binary by the given char.
-spec split_by_char(
    Bin::binary(), Char::binary(), StopAtFirst::boolean()
) -> [binary()].
split_by_char(Bin, Char, StopAtFirst)
    when is_binary(Bin), is_binary(Char), is_boolean(StopAtFirst) ->
    reverse(split_by_char(Bin, Char, StopAtFirst, [], <<>>)).

split_by_char(<<Char:8, Rest/binary>>, <<Char>>, true, AccList, AccWord) ->
    split_by_char_add(Rest, split_by_char_add(AccWord, AccList));

split_by_char(<<Char:8, Rest/binary>>, <<Char>>, false, AccList, AccWord) ->
    NewAccList = split_by_char_add(AccWord, AccList),
    split_by_char(Rest, <<Char>>, false, NewAccList, <<>>);

split_by_char(<<>>, _Char, _StopAtFirst, AccList, AccWord) ->
    split_by_char_add(AccWord, AccList);

split_by_char(<<Byte:8, Rest/binary>>, Char, StopAtFirst, AccList, AccWord) ->
    NewAccWord = <<AccWord/binary, Byte>>,
    split_by_char(Rest, Char, StopAtFirst, AccList, NewAccWord).

split_by_char_add(<<>>, AccList) ->
    AccList;

split_by_char_add(Bin, AccList) ->
    [Bin|AccList].

%% @doc Reverses a list.
-spec reverse(List::[term()]) -> [term()].
%reverse([]) ->
%    [];

reverse(List) when is_list(List) ->
    reverse(List, []).

reverse([], Acc) ->
    Acc;

reverse([H | T], Acc) ->
    reverse(T, [H|Acc]).

%% @doc Silly hex decoding routines.
-spec from_hex(BinDigit::binary()) -> integer().
from_hex(<<"a">>) ->
    10;

from_hex(<<"b">>) ->
    11;

from_hex(<<"c">>) ->
    12;

from_hex(<<"d">>) ->
    13;

from_hex(<<"e">>) ->
    14;

from_hex(<<"f">>) ->
    15;

from_hex(<<Num:8>>) ->
    Num - 48.

%% @doc Decodes an encoded character in the form %NN where N is an hex digit.
-spec decode(Bin::binary()) -> integer().
decode(Bin) ->
    decode(Bin, <<>>).

decode(<<"%", Byte1:8, Byte2:8, Rest/binary>>, Acc) ->
    ByteVal
        = from_hex(lc(<<Byte1>>)) * 16
        + from_hex(lc(<<Byte2>>)),
    decode(Rest, <<Acc/binary, ByteVal>>);

decode(<<>>, Acc) ->
    Acc;

decode(<<Byte:8, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/binary, Byte>>).

%% @doc Given a binary in the form &lt;&lt;"key=value"&gt;&gt;, will return
%% {&lt;&lt;"key"&gt;&gt;, &lt;&lt;"value"&gt;&gt;}.
-spec parse_key_equal_value(Bin::binary()) -> {binary(), binary()}.
parse_key_equal_value(Bin) when is_binary(Bin) ->
    parse_key_equal_value(Bin, <<>>).

parse_key_equal_value(<<>>, <<>>) ->
    none;

parse_key_equal_value(<<>>, Acc) ->
    {decode(Acc), <<>>};

parse_key_equal_value(<<"=", Rest/binary>>, Acc) ->
    {decode(Acc), decode(Rest)};

parse_key_equal_value(<<Byte:8, Rest/binary>>, Acc) ->
    parse_key_equal_value(Rest, <<Acc/binary, Byte>>).

%% @doc Given a binary with key/value variables, will return the
%% variables found. When called with &lt;&lt;"a=b&amp;c=d"&gt;&gt; (and
%% Sep=&amp;) will return a variable set with both variables set.
-spec parse_key_values_by(
    Bin::binary(), Sep::binary()
) -> ehttp_variable:variables().
parse_key_values_by(Bin, Sep) when is_binary(Bin), is_binary(Sep) ->
    lists:foldl(
        fun(VarBin, Acc) ->
            {Key, Value} = parse_key_equal_value(VarBin),
            ehttp_variable:set(Acc, Key, Value)
        end,
        ehttp_variable:new_variables(),
        split_by_char(Bin, Sep, false)
    ).

%% @doc Encodes a string so it can be sent by wire.
-spec encode(Char::binary()) -> binary().
encode(16#24) ->
    <<"%24">>;

encode(16#26) ->
    <<"%26">>;

encode(16#2B) ->
    <<"%2B">>;

encode(16#2C) ->
    <<"%2C">>;

encode(16#2F) ->
    <<"%2F">>;

encode(16#3A) ->
    <<"%3A">>;

encode(16#3B) ->
    <<"%3B">>;

encode(16#3D) ->
    <<"%3D">>;

encode(16#3F) ->
    <<"%3F">>;

encode(16#40) ->
    <<"%40">>;

encode(16#20) ->
    <<"%20">>;

encode(16#22) ->
    <<"%22">>;

encode(16#3C) ->
    <<"%3C">>;

encode(16#3E) ->
    <<"%3E">>;

encode(16#23) ->
    <<"%23">>;

encode(16#25) ->
    <<"%25">>;

encode(16#7B) ->
    <<"%7B">>;

encode(16#7D) ->
    <<"%7D">>;

encode(16#7C) ->
    <<"%7C">>;

encode(16#5C) ->
    <<"%5C">>;

encode(16#5E) ->
    <<"%5E">>;

encode(16#7E) ->
    <<"%7E">>;

encode(16#5B) ->
    <<"%5B">>;

encode(16#5D) ->
    <<"%5D">>;

encode(16#60) ->
    <<"%60">>;

encode(<<>>) ->
    <<>>;

encode(Byte) when Byte >= 0, Byte =< 255 ->
    <<Byte>>;

encode(Bin) when is_binary(Bin) ->
    encode(Bin, <<>>).

encode(<<>>, Acc) ->
    Acc;

encode(<<Byte:8, Rest/binary>>, Acc) ->
    Encoded = encode(Byte),
    encode(Rest, <<Acc/binary, Encoded/binary>>).
