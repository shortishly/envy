%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(envy).

-export([get_env/3]).
-export([make/0]).
-export([start/0]).
-export([to_atom/3]).
-export([to_binary/3]).
-export([to_boolean/3]).
-export([to_float/3]).
-export([to_integer/3]).
-export([to_list/3]).


to_integer(Application, Key, Strategy) ->
    any:to_integer(get_env(Application, Key, Strategy)).

to_float(Application, Key, Strategy) ->
    any:to_float(get_env(Application, Key, Strategy)).

to_atom(Application, Key, Strategy) ->
    any:to_atom(get_env(Application, Key, Strategy)).

to_boolean(Application, Key, Strategy) ->
    any:to_boolean(get_env(Application, Key, Strategy)).

to_binary(Application, Key, Strategy) ->
    any:to_binary(get_env(Application, Key, Strategy)).

to_list(Application, Key, Strategy) ->
    any:to_list(get_env(Application, Key, Strategy)).


%% a wafer thin wrapper on gproc's get_env that ensures any os_env is
%% prefixed by the application name to prevent nasty environment
%% clashes.

get_env(Application, Key, Strategy) ->
    case lists:prefix(
           any:to_list(Application),
           string:to_upper(any:to_list(Key))) of

        true ->
            gproc:get_env(l, Application, Key, Strategy);

        false ->
            gproc:get_env(l, Application, Key, lists:map(
                                                 os_env(Application, Key),
                                                 Strategy))
    end.


os_env(Application, Key) ->
    fun
        (os_env) ->
            {os_env, prefix_with_application(Application, Key)};

        (Otherwise) ->
            Otherwise
    end.


prefix_with_application(Application, Key) ->
    to_upper(Application) ++ "_" ++ to_upper(Key).

to_upper(X) ->
    string:to_upper(any:to_list(X)).

make() ->
    make:all([load]).


start() ->
    application:ensure_all_started(?MODULE).
