%%% @doc HTTP miscelaneous functions.
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
-module(ehttp_http).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-export([marshall/1, unmarshall/1]).
-export([http_v11/0, http_v10/0, http_v09/0]).
-export([marshall_version/1, unmarshall_version/1]).
-export([marshall_method/1, unmarshall_method/1]).
-export([
    http_get/0, http_post/0, http_options/0, http_trace/0, http_delete/0,
    http_put/0, http_head/0
]).

-type version() :: http_v11|http_v10|http_v09.
-type method() ::
    http_post|http_get|http_head|http_put|http_delete
    |http_trace|http_options|http_connect.
-export_type([version/0, method/0]).

%% @doc Returns the end of line.
-spec eol() -> binary().
eol() ->
    <<13,10>>.

% @doc Returns the atom for HTTP version 1.1
-spec http_v11() -> http_v11.
http_v11() ->
    http_v11.

% @doc Returns the atom for HTTP version 1.0
-spec http_v10() -> http_v10.
http_v10() ->
    http_v10.

% @doc Returns the atom for HTTP version 0.9
-spec http_v09() -> http_v09.
http_v09() ->
    http_v09.

% @doc Returns the atom for HTTP method GET.
-spec http_get() -> http_get.
http_get() ->
    http_get.

% @doc Returns the atom for HTTP method POST.
-spec http_post() -> http_post.
http_post() ->
    http_post.

% @doc Returns the atom for HTTP method OPTIONS.
-spec http_options() -> http_options.
http_options() ->
    http_options.

% @doc Returns the atom for HTTP method TRACE.
-spec http_trace() -> http_trace.
http_trace() ->
    http_trace.

% @doc Returns the atom for HTTP method DELETE.
-spec http_delete() -> http_delete.
http_delete() ->
    http_delete.

% @doc Returns the atom for HTTP method PUT.
-spec http_put() -> http_put.
http_put() ->
    http_put.

% @doc Returns the atom for HTTP method HEAD.
-spec http_head() -> http_head.
http_head() ->
    http_head.

% @doc Returns the atom for HTTP method CONNECT.
-spec http_connect() -> http_connect.
http_connect() ->
    http_connect.

%% @doc Adds the end of line to the end of this binary.
-spec marshall(Line::binary()) -> binary().
marshall(Line) when is_binary(Line) ->
    Eol = eol(),
    <<Line/binary, Eol/binary>>.

%% @doc Removes newline (\r\n and \n) from the end of the line.
-spec unmarshall(Line::binary()) -> binary().
unmarshall(<<>>) ->
    <<>>;
unmarshall(Line) when is_binary(Line) ->
    case binary:last(Line) of
        13 -> unmarshall(binary:part(Line, 0, erlang:byte_size(Line)-1));
        10 -> unmarshall(binary:part(Line, 0, erlang:byte_size(Line)-1));
        _ -> Line
    end.

%% @doc Returns the binary for HTTP version X.
-spec marshall_version(Version::version()) -> binary().
marshall_version(http_v11) ->
    <<"1.1">>;
marshall_version(http_v10) ->
    <<"1.0">>;
marshall_version(http_v09) ->
    <<"0.9">>.

%% @doc Returns the atom that corresponds to the given HTTP version.
-spec unmarshall_version(Version::binary()) -> version().
unmarshall_version(<<"1.1">>) ->
    http_v11();
unmarshall_version(<<"1.0">>) ->
    http_v10();
unmarshall_version(<<"0.9">>) ->
    http_v09().

%% @doc Returns the binary that corresponds to the given HTTP method atom.
-spec marshall_method(Method::method()) -> binary().
marshall_method(http_get) ->
    <<"GET">>;
marshall_method(http_post) ->
    <<"POST">>;
marshall_method(http_head) ->
    <<"HEAD">>;
marshall_method(http_put) ->
    <<"PUT">>;
marshall_method(http_options) ->
    <<"OPTIONS">>;
marshall_method(http_trace) ->
    <<"TRACE">>;
marshall_method(http_delete) ->
    <<"DELETE">>;
marshall_method(http_connect) ->
    <<"CONNECT">>.

%% @doc Returns the atom that corresponds to the given HTTP method binary.
-spec unmarshall_method(Version::binary()) -> method().
unmarshall_method(<<"get">>) ->
    http_get();
unmarshall_method(<<"post">>) ->
    http_post();
unmarshall_method(<<"head">>) ->
    http_head();
unmarshall_method(<<"put">>) ->
    http_put();
unmarshall_method(<<"options">>) ->
    http_options();
unmarshall_method(<<"trace">>) ->
    http_trace();
unmarshall_method(<<"delete">>) ->
    http_delete();
unmarshall_method(<<"connect">>) ->
    http_connect().
