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
%%% @doc Tracing hook for CT
%%%
%%% TODO: Add description
%%%
%%%
%%% @end

-module(cth_tracing).

-record(state, {
    config,
    tracer
}).

%% ct callbacks
-export([
    id/1,
    init/2,
    pre_init_per_testcase/3,
    post_end_per_testcase/4
]).

%% @doc Provide a unique id for this CTH.
id(_Opts) ->
    ?MODULE.

%% @doc Initiate a common state. Called before any other callback function.
init(_Id, _Opts) ->
    {ok, #state{}}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC, TestConfig, State) ->
    Config = get_tracer_config(TestConfig),
    Tracer = cth_tracer:new(Config),
    attach_meck_maybe(Config, Tracer),
    cth_tracer:start(Tracer),
    {TestConfig, State#state{config = Config, tracer = Tracer}}.

%% @doc Called after each test case.
post_end_per_testcase(_TC, _TestConfig, Return, State) ->
    #state{config = Config, tracer = Tracer} = State,
    detach_meck_maybe(Config, Tracer),
    cth_tracer:stop(Tracer),
    {Return, State#state{config = undefined, tracer = undefined}}.

%% Local functions

attach_meck_maybe(Config, Tracer) ->
    case get_config(trace_mecked, Config) of
        true ->
            cth_tracer:attach_meck(Tracer);
        false ->
            ok
    end.

detach_meck_maybe(Config, Tracer) ->
    case get_config(trace_mecked, Config) of
        true ->
            cth_tracer:detach_meck(Tracer);
        false ->
            ok
    end.

get_config(trace_mecked, Config) ->
    proplists:is_defined(trace_mecked, Config).

get_tracer_config(Config) ->
    proplists:get_value(cth_tracing, Config, []).
