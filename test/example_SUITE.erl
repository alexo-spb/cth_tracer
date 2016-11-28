%%%============================================================================
%%% Copyright 2016 Aleksei Osin
%%%
%%%    Licensed under the Apache License, Version 2.0 (the "License");
%%%    you may not use this file except in compliance with the License.
%%%    You may obtain a copy of the License at
%%%
%%%        http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%    Unless required by applicable law or agreed to in writing, software
%%%    distributed under the License is distributed on an "AS IS" BASIS,
%%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%    See the License for the specific language governing permissions and
%%%    limitations under the License.
%%%============================================================================

%%% @author Aleksei Osin
%%% @copyright 2016 Aleksei Osin
%%% @doc Trace collecting CT hook usage example

-module(example_SUITE).


-compile(export_all).


init_per_suite(Config) ->
    TracerConfig = [
        {show_trace, all},
        {modules, [
            module_a,
            {module_b, [trace_locals]}
        ]}
    ],
    [{cth_tracer, TracerConfig}, {ct_hooks, [cth_tracer]} | Config].

end_per_suite(_Config) ->
    ok.

all() ->
    [testcase].

testcase(_Config) ->
    module_a:call(ping).
