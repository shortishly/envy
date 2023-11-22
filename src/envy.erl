%% Copyright (c) 2012-2022 Peter Morgan <peter.james.morgan@gmail.com>
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


-export([envy/1]).
-export([envy/2]).
-export([envy/3]).
-export([get_env/3]).
-export([start/0]).
-export([suffix/1]).
-export([to_atom/3]).
-export([to_binary/3]).
-export([to_boolean/3]).
-export([to_float/3]).
-export([to_integer/3]).
-export([to_integer_or_atom/3]).
-export([to_list/3]).


to_integer(Application, Key, Strategy) ->
    any:to_integer(get_env(Application, Key, Strategy)).


to_integer_or_atom(Application, Key, Strategy) ->
    try
        any:to_integer(get_env(Application, Key, Strategy))
    catch
        error:function_clause ->
            any:to_atom(get_env(Application, Key, Strategy))
    end.

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

get_env(Application, Key, FullStrategy) ->
    ?FUNCTION_NAME(Application, Key, FullStrategy, undefined).

get_env(Application, Key, [os_env | T], Default) ->
    case os:getenv(
           string:uppercase(
             lists:concat(
               lists:join(
                 "_",
                 case lists:prefix(
                        any:to_list(Application),
                        string:to_upper(any:to_list(Key))) of

                     true ->
                         [Key];

                     false ->
                         [Application, Key]
                 end)))) of

        false ->
            ?FUNCTION_NAME(Application, Key, T, Default);

        Value ->
            Value
    end;

get_env(Application, Key, [app_env | T], Default) ->
    case application:get_env(Application, Key) of
        undefined ->
            ?FUNCTION_NAME(Application, Key, T, Default);

        {ok, Value} ->
            Value
    end;

get_env(Application, Key, [{default, Default} | T], _) ->
    ?FUNCTION_NAME(Application, Key, T, Default);

get_env(_Application, _Key, [], Default) when is_function(Default) ->
    Default();

get_env(_Application, _Key, [], Default) ->
    Default.


start() ->
    application:ensure_all_started(?MODULE).


-type envy_type() :: atom()
                   | binary()
                   | boolean()
                   | float()
                   | integer()
                   | list().

-type to() :: to_atom
            | to_binary
            | to_boolean
            | to_float
            | to_integer
            | to_integer_or_atom
            | to_list.

-type type_name() :: atom
                   | binary
                   | boolean
                   | float
                   | integer
                   | integer_or_atom
                   | list.

-type thing() :: atom()
               | float()
               | integer()
               | string().

-spec envy(#{default => envy_type(),
             to => to(),
             type => type_name(),
             caller => module(),
             names := [thing()]}) -> envy_type().


envy(#{caller := Caller,
       to := To,
       names := Names,
       default := Default}) ->
    try
        envy:To(application(Caller), snake_case(Names), default(Default))

    catch
        error:badarg ->
            Default
    end;

envy(#{caller := Caller,
       to := To,
       names := Names}) ->
    case get_env(application(Caller), snake_case(Names), [os_env, app_env]) of
        undefined ->
            error(badarg, [To, Names]);

        Value ->
            any:To(Value)
    end;

envy(#{to := To, names := Names, default := Default}) ->
    ?FUNCTION_NAME(To, Names, Default);

envy(#{to := To, names := Names}) ->
    ?FUNCTION_NAME(To, Names);

envy(#{default := infinity} = Arg)
  when not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_integer_or_atom});

envy(#{default := Default} = Arg)
  when is_boolean(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_boolean});

envy(#{default := Default} = Arg)
  when is_atom(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_atom});

envy(#{default := Default} = Arg)
  when is_binary(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_binary});

envy(#{default := Default} = Arg)
  when is_integer(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_integer});

envy(#{default := Default} = Arg)
  when is_list(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_list});

envy(#{default := Default} = Arg)
  when is_float(Default),
       not(is_map_key(to, Arg)),
       not(is_map_key(type, Arg)) ->
    ?FUNCTION_NAME(Arg#{to => to_float});

envy(#{type := Type} = Arg) when Type == atom;
                                 Type == binary;
                                 Type == boolean;
                                 Type == float;
                                 Type == integer;
                                 Type == integer_or_atom;
                                 Type == list;
                                 not(is_map_key(to, Arg)) ->
    ?FUNCTION_NAME(
       maps:merge(
         #{to => snake_case([to, Type])},
         maps:without([type], Arg))).


envy(To, Names) ->
    case get_env(application(), snake_case(Names), [os_env, app_env]) of
        undefined ->
            error(badarg, [To, Names]);

        Value ->
            any:To(Value)
    end.


envy(To, Names, Default) ->
    try
        envy:To(application(), snake_case(Names), default(Default))

    catch
        error:badarg ->
            Default
    end.


application(M) ->
    case string:split(atom_to_list(M), "_") of
        [_] ->
            error(badarg, [M]);

        [Prefix | _] ->
            list_to_atom(Prefix)
    end.

suffix(M) ->
    case string:split(atom_to_list(M), "_") of
        [_] ->
            error(badarg, [M]);

        [_ | Suffix] ->
            snake_case(Suffix)
    end.


application() ->
    {ok, Application} = application:get_application(),
    Application.


snake_case([_ | _] = Labels) ->
    list_to_atom(lists:concat(lists:join("_", Labels))).


default(Default) ->
    %% Enable all configuration to be overriden by OS environment
    %% variables, very useful for Docker.
    [os_env, app_env, {default, Default}].
