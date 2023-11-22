%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(envy_gen_tests).


-include_lib("eunit/include/eunit.hrl").


options_test_() ->
    lists:map(
      fun
          ({AppVars, Module, Expected}) ->
              {setup,
               setup(AppVars),
               fun cleanup/1,
               ?_assertEqual(
                  Expected,
                  envy_gen:options(Module))}
      end,
      [{[{abc, def_log, true}], abc_def, [{debug, [log]}]},

       {[{abc, def_log_n, 6}], abc_def, []},

       {[{abc, def_log, true},
         {abc, def_log_n, 6}],
        abc_def,
        [{debug, [{log, 6}]}]},

       {[{abc, def_trace, true}], abc_def, [{debug, [trace]}]},

       {[{abc, def_trace, true},
         {abc, def_log, true}],
        abc_def,
        [{debug, [trace, log]}]},

       {[{abc, def_trace, true},
         {abc, def_log_n, 6},
         {abc, def_log, true}],
        abc_def,
        [{debug, [trace, {log, 6}]}]},

       {[{abc, def_hibernate_after, 6}], abc_def, [{hibernate_after, 6}]},

       {[{pqr, def_trace, true},
         {pqr, def_log_n, 6},
         {pqr, def_log, true},
         {pqr, def_hibernate_after, 6}],
        abc_def,
        []},

       {[{abc, pqr_trace, true},
         {abc, pqr_log_n, 6},
         {abc, pqr_log, true},
         {abc, pqr_hibernate_after, 6}],
        abc_def,
        []},

       {[], abc_def, []}]).


setup(KV) ->
    fun
        () ->
            lists:map(
              fun
                  ({Application, Key, Value}) ->
                      application:set_env(Application, Key, Value),
                      {Application, Key}
              end,
              KV)
    end.


cleanup(AppKeys) ->
    lists:foreach(
      fun
          ({Application, Key}) ->
              application:unset_env(Application, Key)
      end,
      AppKeys).
