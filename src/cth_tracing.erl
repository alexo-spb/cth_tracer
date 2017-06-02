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

%% ct callbacks
-export([
    id/1,
    init/2,
    post_init_per_testcase/4,
    pre_end_per_testcase/3
]).

%% @doc Provide a unique id for this CTH.
id(_Opts) ->
    ?MODULE.

%% @doc Initiate a common state. Called before any other callback function.
init(_Id, _Opts) ->
    {ok, undefined}.

%% @doc Called before each test case.
post_init_per_testcase(_TC, TestConfig, _Return, State) ->
    case make_config(TestConfig) of
        undefined ->
            {TestConfig, State};
        Config ->
            Tracer = cth_tracer:new(Config),
            cth_tracer:start(Tracer),
            {TestConfig, Tracer}
    end.

%% @doc Called after each test case.
pre_end_per_testcase(_TC, TestConfig, undefined) ->
    {TestConfig, undefined};
pre_end_per_testcase(_TC, TestConfig, Tracer) ->
    cth_tracer:stop(Tracer),
    {TestConfig, undefined}.

%% Local functions

make_config(TestConfig) ->
    case proplists:get_value(cth_tracing, TestConfig) of
        undefined ->
            undefined;
        HookConfig ->
            make_config(HookConfig, [])
    end.

make_config([{procs, _} = H | T], Config) ->
    make_config(T, [H | Config]);
make_config([{flags, _} = H | T], Config) ->
    make_config(T, [H | Config]);
make_config([{trace_opts, _} = H | T], Config) ->
    make_config(T, [H | Config]);
make_config([trace_mecked = H | T], Config) ->
    make_config(T, [H | Config]);
make_config([{modules, _} = H | T], Config) ->
    make_config(T, [H | Config]);
make_config([{out, _} = H | T], Config) ->
    Opts = proplists:get_value(format_opts, Config, []),
    Tuple = {format_opts, [H | Opts]},
    make_config(T, lists:keystore(format_opts, 1, Config, Tuple));
make_config([{handler, {M, F, A}} | T], Config) ->
    Opts = proplists:get_value(format_opts, Config, []),
    Tuple = {format_opts, [{handler, apply(M, F, A)} | Opts]},
    make_config(T, lists:keystore(format_opts, 1, Config, Tuple));
make_config([_ | T], Config) ->
    make_config(T, Config);
make_config([], []) ->
    undefined;
make_config([], Config) ->
    Config.

