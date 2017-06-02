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
    [{ct_hooks, [cth_tracing]} | Config].

init_per_testcase(_TC, Config) ->
    Handler = {
        cth_tracer_puml_format, get_handler,
        [[{plantuml, "~/devel/plantuml.jar"}]]
    },
    TracerConfig = [
        {out, "example_SUITE.testcase.trace.html"},
        {handler, Handler},
        {modules, [
            module_a,
            {module_b, [trace_locals]}
        ]}
    ],
    [{cth_tracing, TracerConfig} | Config].

end_per_testcase(_TC, _Config) ->
    ok.

all() ->
    [testcase].

testcase(Config) ->
    module_a:call(ping).
