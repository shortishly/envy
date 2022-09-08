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


-module(envy_tests).


-include_lib("eunit/include/eunit.hrl").


empty_test_() ->
    Application = abc,
    Key = pqr,
    {setup,
     fun envy:start/0,
     fun
         (_) ->
             application:stop(envy)
     end,
     [?_assertEqual(
         undefined,
         envy:to_atom(
           Application,
           Key,
           [])),

     ?_assertEqual(
        "undefined",
         envy:to_list(
           Application,
           Key,
           [])),

     ?_assertEqual(
        <<"undefined">>,
         envy:to_binary(
           Application,
           Key,
           []))]}.


undefined_test_() ->
    Application = abc,
    Key = pqr,
    {setup,
     fun envy:start/0,
     fun
         (_) ->
             application:stop(envy)
     end,
     [?_assertEqual(
         undefined,
         envy:to_atom(
           Application,
           Key,
           [os_env])),

     ?_assertEqual(
        undefined,
         envy:to_atom(
           Application,
           Key,
           [app_env]))]}.


default_test_() ->
    Application = abc,
    Key = pqr,
    Default = 6,
    {setup,
     fun envy:start/0,
     fun
         (_) ->
             application:stop(envy)
     end,
     [?_assertEqual(
         Default,
         envy:to_integer(
           Application,
           Key,
           [os_env, app_env, {default, Default}])),

      ?_assertEqual(
         Default,
         envy:to_integer(
           Application,
           Key,
           [os_env,
            app_env,
            {default,
             fun
                 () ->
                     Default
             end}]))]}.


default_only_test_() ->
    Application = abc,
    Key = pqr,
    Default = 6,
    {setup,
     fun envy:start/0,
     fun
         (_) ->
             application:stop(envy)
     end,
     ?_assertEqual(
        Default,
        envy:to_integer(
          Application,
          Key,
          [{default, Default}]))}.

app_env_test_() ->
    Application = abc,
    Key = pqr,
    Default = 6,
    AppVal = 123,
    {setup,
     fun
         () ->
             _ = envy:start(),
             application:set_env(Application, Key, AppVal)
     end,
     fun
         (_) ->
             application:stop(envy),
             application:unset_env(Application, Key)
     end,
     ?_assertEqual(
        AppVal,
        envy:to_integer(
          Application,
          Key,
          [os_env, app_env, {default, Default}]))}.


os_env_test_() ->
    Application = abc,
    Key = pqr,
    Default = 6,
    EnvVal = 321,
    {setup,
     fun
         () ->
             _ = envy:start(),
             os:putenv(env_var(Application, Key), integer_to_list(EnvVal))
     end,
     fun
         (_) ->
             application:stop(envy),
             os:unsetenv(env_var(Application, Key))
     end,
     ?_assertEqual(
        EnvVal,
        envy:to_integer(
          Application,
          Key,
          [os_env, app_env, {default, Default}]))}.


combo_test_() ->
    Application = abc,
    Key = pqr,
    Default = 6,
    AppVal = 123,
    EnvVal = 321,
    {setup,
     fun
         () ->
             _ = envy:start(),
             application:set_env(Application, Key, AppVal),
             os:putenv(env_var(Application, Key), integer_to_list(EnvVal))
     end,
     fun
         (_) ->
             application:stop(envy),
             application:unset_env(Application, Key),
             os:unsetenv(env_var(Application, Key))
     end,
     [?_assertEqual(
        EnvVal,
        envy:to_integer(
          Application,
          Key,
          [os_env, app_env, {default, Default}])),

      ?_assertEqual(
         AppVal,
         envy:to_integer(
           Application,
           Key,
           [app_env, os_env, {default, Default}])),

      ?_assertEqual(
         AppVal,
         envy:to_integer(
           Application,
           Key,
           [{default, Default}, app_env, os_env])),

      ?_assertEqual(
         EnvVal,
         envy:to_integer(
           Application,
           Key,
           [{default, Default}, os_env, app_env]))]}.


env_var(Application, Key) ->
    string:uppercase(lists:concat(lists:join("_", [Application, Key]))).
