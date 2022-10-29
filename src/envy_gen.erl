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


-module(envy_gen).


-export([options/1]).
-import(envy, [envy/2]).
-import(envy, [envy/3]).


options(M) ->
    case debug_options(application_module_suffix(M), [log, trace]) of
        [] ->
            [];

        Options ->
            [{debug, Options}]
    end.

application_module_suffix(M) ->
    {ok, Application} = application:get_application(),
    case string:prefix(atom_to_binary(M), atom_to_binary(Application)) of
        <<"_", Suffix/bytes>> ->
            binary_to_atom(Suffix);

        nomatch ->
            M
    end.

debug_options(M, Options) ->
    ?FUNCTION_NAME(M, Options, []).


debug_options(M, [log = Option | Options], A) ->
    case envy(to_boolean, [M, Option], false) of
        true ->
            try
                ?FUNCTION_NAME(
                   M,
                   Options,
                   [{log, envy(to_integer, [M, Option, n])} | A])
            catch
                error:badarg ->
                    ?FUNCTION_NAME(
                       M,
                       Options,
                       [log | A])

            end;

        false ->
            ?FUNCTION_NAME(M, Options, A)
    end;

debug_options(M, [Option | Options], A) ->
    ?FUNCTION_NAME(
       M,
       Options,
       [Option || envy(to_boolean, [M, Option], false)] ++ A);

debug_options(_, [], A) ->
    A.
