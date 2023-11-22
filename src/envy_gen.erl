%% Copyright (c) 2012-2023 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(envy_gen).


-export([options/1]).
-include_lib("kernel/include/logger.hrl").


options(M) ->
    Options = lists:append([debug_options(M), hibernate_after(M)]),
    ?LOG_DEBUG(#{M => Options}),
    Options.


debug_options(M) ->
    case ?FUNCTION_NAME(M, [log, trace]) of
        [] ->
            [];

        Options ->
            [{debug, Options}]
    end.

debug_options(M, Options) ->
    ?FUNCTION_NAME(M, envy:suffix(M), Options, []).


debug_options(M, Suffix, [log = Option | Options], A) ->
    case envy:envy(#{caller => M,
                     default => false,
                     names => [Suffix, Option]}) of
        true ->
            try
                ?FUNCTION_NAME(
                   M,
                   Suffix,
                   Options,
                   [{log,
                     envy:envy(
                       #{caller => M,
                         to => to_integer,
                         names => [Suffix, Option, n]})} | A])
            catch
                error:badarg ->
                    ?FUNCTION_NAME(
                       M,
                       Suffix,
                       Options,
                       [log | A])

            end;

        false ->
            ?FUNCTION_NAME(M, Suffix, Options, A)
    end;

debug_options(M, Suffix, [Option | Options], A) ->
    ?FUNCTION_NAME(
       M,
       Suffix,
       Options,
       [Option || envy:envy(
                    #{caller => M,
                      names => [Suffix, Option],
                      default => false})] ++ A);

debug_options(_, _, [], A) ->
    A.


hibernate_after(M) ->
    try
        [{?FUNCTION_NAME,
          envy:envy(
            #{caller => M,
              to => to_integer,
              names => [envy:suffix(M), ?FUNCTION_NAME]})}]

    catch
        error:badarg ->
            []
    end.
