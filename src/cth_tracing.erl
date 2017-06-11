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
    post_init_per_suite/4,
    post_init_per_testcase/4,
    pre_end_per_testcase/3
]).

-record(state, {
    suite :: atom(),
    tracer
}).

%% @doc Provide a unique id for this CTH.
id(_Opts) ->
    ?MODULE.

%% @doc Initiate a common state. Called before any other callback function.
init(_Id, _Opts) ->
    {ok, undefined}.

%% @doc Called after init_per_suite.
post_init_per_suite(Suite, Config, Return, _) ->
     State = init_state(Suite),
     {Return, State}.

%% @doc Called before each test case.
post_init_per_testcase(TC, TestConfig, _Return, State0) ->
    State1 = start_tracer(TC, TestConfig, State0),
    {TestConfig, State1}.

%% @doc Called after each test case.
pre_end_per_testcase(_TC, TestConfig, State0) ->
    State1 = stop_tracer(State0),
    {TestConfig, State1}.

%% Local functions

init_state(Suite) ->
    #state{suite = Suite}.

start_tracer(TC, TestConfig, State) ->
    #state{suite = Suite} = State,
    case make_config(Suite, TC, TestConfig) of
        [] ->
            State;
        Config ->
            Tracer = cth_tracer:new(Config),
            cth_tracer:start(Tracer),
            State#state{tracer = Tracer}
    end.

stop_tracer(State) ->
    #state{tracer = Tracer} = State,
    case Tracer of
        undefined ->
            State;
        _ ->
            cth_tracer:stop(Tracer),
            State#state{tracer = undefined}
    end.

make_config(Suite, TC, TestConfig) ->
    case proplists:get_value(cth_tracing, TestConfig) of
        undefined ->
            [];
        HookConfig ->
            case make_config(HookConfig, []) of
                [] ->
                    [];
                Config ->
                    prefix_config(Suite, TC, Config)
            end
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

prefix_config(Suite, TC, Config) ->
    Prefix = make_prefix(Suite, TC),
    prefix_out(Prefix, prefix_fetch_dir(Prefix, Config)).

make_prefix(Suite, TC) ->
    lists:concat([Suite, ".", TC, "."]).

prefix_fetch_dir(Prefix, Config) ->
    FetchDir = proplists:get_value(fetch_dir, Config, "trace"),
    lists:keystore(fetch_dir, 1, Config, {fetch_dir, Prefix ++ FetchDir}).

prefix_out(Prefix, Config) ->
    Opts0 = proplists:get_value(format_opts, Config, []),
    Out = proplists:get_value(out, Opts0, "out"),
    Opts1 = lists:keystore(out, 1, Opts0, {out, Prefix ++ Out}),
    lists:keystore(format_opts, 1, Config, {format_opts, Opts1}).
