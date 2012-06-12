%%% @doc This generically represents a variable and a set of variables. Pretty
%%% much like dict, but only with binaries for keys and values.
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
-module(ehttp_variable).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-type variable_key() :: binary().
-type variable_value() :: binary().
-type variable_values() :: [variable_value()].
-type variable() :: {variable_key(), variable_value()}.
-type variables() :: [variable()].
-export_type([variable/0, variables/0, variable_key/0, variable_value/0]).

-export([new_variables/0, get/2, set/3, has/2]).

%% @doc Creates a new set of variables.
-spec new_variables() -> variables().
new_variables() ->
    [].

%% @doc Sets a variable value. If the variable does not exist, it will be added.
%% If the variable already exists, the value will be replaced.
-spec set(
    Variables::variables(), Key::variable_key(),
    Value::variable_value()|variable_values()
) -> variables().
set(Variables, Key, Value)
    when is_list(Variables), is_binary(Key) ->
    % Call the tail recursive version.
    ehttp_bin:reverse(set(Variables, Key, Value, new_variables(), false)).

set([], Key, Value, [], _Added) ->
    [{Key, Value}];

set([], _Key, _Value, Acc, true) ->
    Acc;

set([], Key, Value, Acc, false) ->
    % The complete list was traversed and the variable was not found, so let's
    % add it as a new one.
    [{Key, Value} | Acc];

set([{Key,_OldValue} | T], Key, Value, Acc, _Added) ->
    % The variable already exists, add it to the list with a new value.
    set(T, Key, Value, [{Key, Value} | Acc], true);

set([{OldKey,OldValue} | T], Key, Value, Acc, Added) ->
    % This variable is not the one we're looking for, add it to the new list
    % and continue traversing the list.
    set(T, Key, Value, [{OldKey, OldValue} | Acc], Added).

%% @doc Returns a variable value. Will stop traversing the variable list when
%% found.
-spec get(
    Variables::variables(), Key::variable_key()
) -> variable_value()|variable_values() | notfound.
get([], _Key) ->
    % The list was traversed without finding the variable.
    notfound;

get([{Key, Value} | _], Key) when is_binary(Key) ->
    % If the variable is found, stop traversing the list and return the value.
    Value;

get([{Key, _Value} | T], SearchKey) when is_binary(Key) ->
    % The variable did not match, continue traversing the list.
    get(T, SearchKey).

%% @doc true if the given variable list contains the given variable.
-spec has(Variables::variables(), Key::variable_key()) -> boolean().
has(Variables, Key) ->
    get(Variables, Key) =/= notfound.

